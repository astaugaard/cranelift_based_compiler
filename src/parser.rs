use std::{collections::HashMap, rc::Rc};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::{complete::{self, multispace0}},
    combinator::value,
    error::ErrorKind,
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, terminated},
    IResult, InputTakeAtPosition,
};
use once_cell::sync::Lazy;

use crate::ast::*;

pub fn parse(input: &str) -> IResult<&str, Ast> {
    preceded(multispace0, many0(definition))(input)
}

fn definition(input: &str) -> IResult<&str, Definitions> {
    dbg!(delimited(
        terminated(token("("), token("fun")),
        definition_body,
        token(")"),
    )(input))
}

fn definition_body(input: &str) -> IResult<&str, Definitions> {
    let (remaining, name) = identifier(input)?;

    let (remaining, args) =
        delimited(token("("), separated_list0(token(","), arg), token(")"))(remaining)?;

    println!("done with args");

    let (remaining, expr) = expr(remaining)?;

    println!("done with expr");

    Ok((remaining, Definitions::Defun(name, args, expr)))
}

fn arg(input: &str) -> IResult<&str, (String, Rc<Type>)> {
    pair(identifier, parse_type)(input)
}

// todo make function type parsing better
fn parse_type(input: &str) -> IResult<&str, Rc<Type>> {
    let (remaining, t) = alt((
        value(Type::I32, token("i32")),
        // map(
        //     delimited(
        //         token("("),
        //         preceded(token("->"), many1(parse_type)),
        //         token(")"),
        //     ),
        //     |args| Type::Func(args),
        // ),
    ))(input)?;

    Ok((remaining, Rc::new(t)))
}

fn expr(input: &str) -> IResult<&str, Rc<Expr>> {
    dbg!(alt((
        parse_let,
        parse_application,
        number,
        identifier_or_primitive,
    ))(input))
}

fn number(input: &str) -> IResult<&str, Rc<Expr>> {
    let (not_parsed, num) = complete::i32(input)?;

    let (not_parsed, _) = multispace0(not_parsed)?;

    return Ok((not_parsed, Rc::new(Expr::Value(num))));
}

static PRIMITIVES: Lazy<HashMap<String, Primitive>> = Lazy::new(|| {
    let mut map = HashMap::new();

    map.insert("+".to_string(), Primitive::Add);
    map.insert("print".to_string(), Primitive::Print);
    map.insert("get".to_string(), Primitive::Get);
    map.insert("-".to_string(), Primitive::Sub);
    map.insert("*".to_string(), Primitive::Mult);
    map.insert("neg".to_string(), Primitive::Neg);
    map.insert("/".to_string(), Primitive::Div);
    map.insert("%".to_string(), Primitive::Mod);
    map.insert(">".to_string(), Primitive::GT);
    map.insert("<".to_string(), Primitive::LT);
    map.insert("<=".to_string(), Primitive::LE);
    map.insert(">=".to_string(), Primitive::GE);
    map.insert("=".to_string(), Primitive::EQ);
    map.insert("if".to_string(), Primitive::IF);

    map
});

fn identifier_or_primitive(input: &str) -> IResult<&str, Rc<Expr>> {
    let (rest, name) = identifier(input)?;

    let res = match Lazy::force(&PRIMITIVES).get(&name) {
        Some(p) => Expr::Primitive(*p),
        None => Expr::Identifier(name),
    };

    Ok((rest, Rc::new(res)))
}

fn parse_let(input: &str) -> IResult<&str, Rc<Expr>> {
    let (rest, ((name, value), result)) = delimited(
        token("("),
        preceded(
            token("let"),
            pair(
                delimited(token("("), pair(identifier, expr), token(")")),
                expr,
            ),
        ),
        token(")"),
    )(input)?;

    Ok((rest, Rc::new(Expr::Let(name, value, result))))
}

fn parse_application(input: &str) -> IResult<&str, Rc<Expr>> {
    let (rest, (fun, args)) = delimited(token("("), pair(expr, many0(expr)), token(")"))(input)?;

    Ok((rest, Rc::new(Expr::App(fun, args))))
}

fn identifier(input: &str) -> IResult<&str, String> {
    let (remaining, name) = input.split_at_position1_complete(
        |c| !(c.is_alphanumeric() | is_one_of(c, "+-/%=!@#$%^&*\\?")),
        dbg!(ErrorKind::MultiSpace),
    )?;

    let (remaining, _) = multispace0(remaining)?;

    Ok((remaining, name.to_string()))
}

fn is_one_of(c: char, chars: &str) -> bool {
    for i in chars.chars() {
        if c == i {
            return true;
        }
    }

    return false;
}

fn token<'a>(arg: &'a str) -> impl Fn(&str) -> IResult<&str, ()> + 'a {
    move |input| value((), terminated(tag(arg), whitespace))(input)
}

fn whitespace(input: &str) -> IResult<&str, ()> {
    value((), multispace0)(input)
}
