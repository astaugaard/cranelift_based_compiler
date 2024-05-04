use std::{collections::BTreeMap, rc::Rc};

use cranelift::{
    codegen::{
        ir::{
            self, condcodes::IntCC, types, AbiParam, Block, FuncRef, Function, InstBuilder,
            Signature, UserExternalName, UserFuncName, Value,
        },
        isa,
        settings::{self, Configurable},
        Context,
    },
    frontend::{FunctionBuilder, FunctionBuilderContext},
};
use cranelift_module::{FuncId, Linkage, Module};
use cranelift_object::{object::Object, ObjectBuilder, ObjectModule, ObjectProduct};
use target_lexicon::Triple;

use crate::{
    anf::{Anf, AnfClause, AnfDefinition, AnfVal},
    ast::Primitive,
};

struct CompileContext {
    object: ObjectModule,
    context: Context,
    // signature: Signature,
    func_builder: FunctionBuilderContext,
    functions: Vec<Option<(FuncId, Option<Function>)>>,

    print_function: FuncId,
    get_function: FuncId,
}

impl CompileContext {
    fn allocate_functions(&mut self, anf: &Anf) -> Result<(), String> {
        for def in anf {
            match def {
                crate::anf::AnfDefinition::Function(id, name, args, _, _) => {
                    let mut signature = self.object.make_signature();

                    for i in 0..*args {
                        signature.params.push(AbiParam::new(types::I32));
                    }

                    signature.returns.push(AbiParam::new(types::I32));

                    let fid = self
                        .object
                        .declare_function(name, Linkage::Export, &signature)
                        .map_err(|err| err.to_string())?;

                    let function = Function::with_name_signature(
                        UserFuncName::User(UserExternalName {
                            namespace: 0,
                            index: *id as u32,
                        }),
                        signature,
                    );

                    self.functions[*id] = Some((fid, Some(function)));
                }
            }
        }

        Ok(())
    }

    fn build_all(&mut self, anf: &[AnfDefinition]) -> Result<(), String> {
        for def in anf {
            self.build(def)?;
        }

        Ok(())
    }

    fn build(&mut self, def: &AnfDefinition) -> Result<(), String> {
        match def {
            AnfDefinition::Function(id, _, args, (clauses, retval), num) => {
                println!("compiling function");

                let (fid, func) = self.functions[*id]
                    .take()
                    .expect("should always be allocated");

                let mut func = func.expect("are compiling now");

                let rec_ref = self.object.declare_func_in_func(fid, &mut func);

                let mut funcid_to_ref: BTreeMap<usize, FuncRef> = BTreeMap::new();
                let mut primitive_to_ref: BTreeMap<Primitive, FuncRef> = BTreeMap::new();

                funcid_to_ref.insert(*id, rec_ref);

                self.declare_funcs_in_def(
                    def,
                    &mut funcid_to_ref,
                    &mut primitive_to_ref,
                    &mut func,
                )?;

                let mut function_builder = FunctionBuilder::new(&mut func, &mut self.func_builder);

                let mut vals: Vec<Option<Value>> = Vec::new();

                vals.resize_with(*num, || None);

                let initial_block = function_builder.create_block();

                function_builder.append_block_params_for_function_params(initial_block);
                function_builder.seal_block(initial_block);
                function_builder.switch_to_block(initial_block);

                for (arg, vals) in vals.iter_mut().enumerate().take(*args) {
                    let val = function_builder.block_params(initial_block)[arg];
                    *vals = Some(val);
                }

                Self::build_clauses(
                    clauses,
                    &mut function_builder,
                    // &mut current_block,
                    &mut vals,
                    &funcid_to_ref,
                    &primitive_to_ref,
                )?;

                let ret = Self::get_as_value(retval, &mut function_builder, &vals);

                function_builder.ins().return_(&[ret]);

                function_builder.finalize();

                // Self::return_val(&clauses, &mut function_builder, current_block, retval);

                self.functions[*id] = Some((fid, None));

                self.context.clear();

                self.context.func = func;

                self.object.define_function(fid, &mut self.context).map_err(|err| err.to_string())?;
            }
        }

        Ok(())
    }

    fn build_clauses(
        clauses: &[Rc<AnfClause>],
        function_builder: &mut FunctionBuilder<'_>,
        // current_block: &mut Block,
        vals: &mut Vec<Option<Value>>,
        func_map: &BTreeMap<usize, FuncRef>,
        prim_map: &BTreeMap<Primitive, FuncRef>,
    ) -> Result<(), String> {
        for c in clauses {
            Self::build_clause(c, function_builder, vals, func_map, prim_map)?;
        }

        Ok(())
    }

    fn build_clause(
        c: &AnfClause,
        function_builder: &mut FunctionBuilder<'_>,
        // current_block: &mut Block,
        vals: &mut Vec<Option<Value>>,
        func_map: &BTreeMap<usize, FuncRef>,
        prim_map: &BTreeMap<Primitive, FuncRef>,
    ) -> Result<(), String> {
        match c {
            AnfClause::LetApp(id, _, fun, args) => {
                let ret = match fun.as_ref() {
                    AnfVal::Var(_) => todo!(),
                    AnfVal::Func(id) => {
                        let funref = func_map.get(id).expect("was inserted");

                        let args: Vec<Value> = args
                            .iter()
                            .map(|arg| Self::get_as_value(arg, function_builder, vals))
                            .collect();

                        let inst = function_builder.ins().call(*funref, &args);
                        function_builder.inst_results(inst)[0]
                    }
                    AnfVal::Primitive(prim) => match prim {
                        Primitive::Print => {
                            let print_ref =
                                prim_map.get(prim).expect("should be in map from search");

                            assert!(args.len() == 1);

                            let arg = Self::get_as_value(args[0].as_ref(), function_builder, vals);

                            function_builder.ins().call(*print_ref, &[arg]);
                            function_builder.ins().iconst(types::I32, 0)
                        }
                        Primitive::Get => {
                            assert!(args.is_empty());

                            let get_ref = prim_map.get(prim).expect("should be in map from search");

                            let inst = function_builder.ins().call(*get_ref, &[]);
                            function_builder.inst_results(inst)[0]
                        }
                        Primitive::Add => Self::apply_2args(
                            |builder, a1, a2| builder.ins().iadd(a1, a2),
                            function_builder,
                            args,
                            vals,
                        ),
                        Primitive::Sub => Self::apply_2args(
                            |builder, a1, a2| builder.ins().isub(a1, a2),
                            function_builder,
                            args,
                            vals,
                        ),
                        Primitive::Mult => Self::apply_2args(
                            |builder, a1, a2| builder.ins().imul(a1, a2),
                            function_builder,
                            args,
                            vals,
                        ),
                        Primitive::Neg => {
                            assert!(args.len() == 1);
                            let a1 = Self::get_as_value(args[0].as_ref(), function_builder, vals);

                            function_builder.ins().ineg(a1)
                        }
                        Primitive::Div => Self::apply_2args(
                            |builder, a1, a2| builder.ins().sdiv(a1, a2),
                            function_builder,
                            args,
                            vals,
                        ),
                        Primitive::Mod => Self::apply_2args(
                            |builder, a1, a2| builder.ins().srem(a1, a2),
                            function_builder,
                            args,
                            vals,
                        ),
                        Primitive::GT => Self::compare_ir_values(
                            IntCC::SignedGreaterThan,
                            args,
                            vals,
                            function_builder,
                        ),
                        Primitive::GE => Self::compare_ir_values(
                            IntCC::SignedGreaterThanOrEqual,
                            args,
                            vals,
                            function_builder,
                        ),
                        Primitive::LE => Self::compare_ir_values(
                            IntCC::SignedLessThanOrEqual,
                            args,
                            vals,
                            function_builder,
                        ),
                        Primitive::LT => Self::compare_ir_values(
                            IntCC::SignedLessThan,
                            args,
                            vals,
                            function_builder,
                        ),
                        Primitive::IF => panic!("this should never happen"),
                        Primitive::EQ => {
                            Self::compare_ir_values(IntCC::Equal, args, vals, function_builder)
                        }
                    },
                    AnfVal::Const(_) => todo!(),
                };

                vals[*id] = Some(ret);
            }
            AnfClause::LetIf(id, _, cond, (t_clauses, t_ret), (f_clauses, f_ret)) => {
                let cond = Self::get_as_value(cond, function_builder, vals);

                let true_block = function_builder.create_block();
                let false_block = function_builder.create_block();
                let continue_block = function_builder.create_block();

                function_builder.append_block_param(continue_block, types::I32);

                function_builder
                    .ins()
                    .brif(cond, true_block, &[], false_block, &[]);

                function_builder.seal_block(true_block);
                function_builder.seal_block(false_block);

                function_builder.switch_to_block(true_block);

                Self::build_clauses(t_clauses, function_builder, vals, func_map, prim_map)?;
                let t_ret = Self::get_as_value(t_ret, function_builder, vals);

                function_builder.ins().jump(continue_block, &[t_ret]);

                function_builder.switch_to_block(false_block);

                Self::build_clauses(f_clauses, function_builder, vals, func_map, prim_map)?;
                let f_ret = Self::get_as_value(f_ret, function_builder, vals);
                function_builder.ins().jump(continue_block, &[f_ret]);

                function_builder.seal_block(continue_block);

                function_builder.switch_to_block(continue_block);

                let res = function_builder.block_params(continue_block)[0];

                vals[*id] = Some(res);
            }
        }

        Ok(())
    }

    fn declare_funcs_in_def(
        &mut self,
        def: &AnfDefinition,
        map: &mut BTreeMap<usize, FuncRef>,
        prim_map: &mut BTreeMap<Primitive, FuncRef>,
        func: &mut Function,
    ) -> Result<(), String> {
        match def {
            AnfDefinition::Function(_, _, _, (clauses, _), _) => {
                for c in clauses {
                    self.declare_funcs_in_clause(c, map, prim_map, func)?;
                }
            }
        };

        Ok(())
    }

    fn declare_funcs_in_clause(
        &mut self,
        c: &AnfClause,
        map: &mut BTreeMap<usize, FuncRef>,
        prim_map: &mut BTreeMap<Primitive, FuncRef>,
        func: &mut Function,
    ) -> Result<(), String> {
        match c {
            AnfClause::LetApp(_, _, fun, _) => match fun.as_ref() {
                AnfVal::Var(_) => {}
                AnfVal::Func(id) => {
                    if !map.contains_key(id) {
                        let refed = self.functions[*id].as_ref().expect("should be allocated");

                        let func_ref = self.object.declare_func_in_func(refed.0, func);

                        map.insert(*id, func_ref);
                    }
                }
                AnfVal::Primitive(prim) => match prim {
                    Primitive::Print => {
                        if !prim_map.contains_key(prim) {
                            let func_ref =
                                self.object.declare_func_in_func(self.print_function, func);

                            prim_map.insert(*prim, func_ref);
                        }
                    }
                    Primitive::Get => {
                        if !prim_map.contains_key(prim) {
                            let func_ref =
                                self.object.declare_func_in_func(self.get_function, func);

                            prim_map.insert(*prim, func_ref);
                        }
                    }
                    _ => (),
                },
                AnfVal::Const(_) => {}
            },
            AnfClause::LetIf(_, _, _, (t,_), (f,_)) => {

                for i in t {
                    self.declare_funcs_in_clause(i, map, prim_map, func)?;
                }

                for i in f {
                    self.declare_funcs_in_clause(i, map, prim_map, func)?;
                }

            },
        }

        Ok(())
    }

    fn get_as_value(
        arg: &AnfVal,
        function_builder: &mut FunctionBuilder<'_>,
        vals: &[Option<Value>],
    ) -> Value {
        match arg {
            AnfVal::Var(v) => vals[*v].expect("should be in vec by now"),
            AnfVal::Func(_) => todo!(),
            AnfVal::Primitive(_) => todo!(),
            AnfVal::Const(i) => function_builder.ins().iconst(types::I32, *i as i64),
        }
    }

    fn compare_ir_values(
        signed_greater_than_or_equal: IntCC,
        args: &[Rc<AnfVal>],
        vals: &[Option<Value>],
        function_builder: &mut FunctionBuilder<'_>,
    ) -> Value {
        assert!(args.len() == 2);

        let a1 = Self::get_as_value(args[0].as_ref(), function_builder, vals);
        let a2 = Self::get_as_value(args[1].as_ref(), function_builder, vals);

        let res = function_builder
            .ins()
            .icmp(signed_greater_than_or_equal, a1, a2);

        let one = function_builder.ins().iconst(types::I32, 1);
        let zero = function_builder.ins().iconst(types::I32, 0);

        function_builder.ins().select(res, one, zero)
    }

    fn apply_2args<F>(
        f: F,
        function_builder: &mut FunctionBuilder<'_>,
        args: &[Rc<AnfVal>],
        vals: &[Option<Value>],
    ) -> Value
    where
        F: FnOnce(&mut FunctionBuilder, Value, Value) -> Value,
    {
        assert!(args.len() == 2);

        let a1 = Self::get_as_value(args[0].as_ref(), function_builder, vals);
        let a2 = Self::get_as_value(args[1].as_ref(), function_builder, vals);

        f(function_builder, a1, a2)
    }
}

pub fn compile(anf: Anf, num_delcs: usize) -> Result<ObjectProduct, String> {
    let mut settings_builder = settings::builder();

    settings_builder.enable("is_pic").unwrap();

    let flags = settings::Flags::new(settings_builder);

    let isa = match isa::lookup(Triple::host()) {
        Ok(a) => a.finish(flags).unwrap(),
        Err(err) => Err(err.to_string())?,
    };

    let builder = ObjectBuilder::new(isa, "main", cranelift_module::default_libcall_names())
        .map_err(|s| s.to_string())?;

    let mut object_module = ObjectModule::new(builder);

    let context = object_module.make_context();

    let func_ctx = FunctionBuilderContext::new();

    let mut func_id_map: Vec<Option<(FuncId, Option<Function>)>> = Vec::new();

    func_id_map.resize_with(num_delcs, || None);

    let mut print_sig = object_module.make_signature();

    print_sig.params.push(AbiParam::new(types::I32));

    let print_function = object_module
        .declare_function("print_num", Linkage::Import, &print_sig)
        .map_err(|err| err.to_string())?;

    object_module.clear_signature(&mut print_sig);

    let mut get_sig = print_sig;

    get_sig.returns.push(AbiParam::new(types::I32));

    let get_function = object_module
        .declare_function("get_num", Linkage::Import, &get_sig)
        .map_err(|err| err.to_string())?;

    let mut comp_context = CompileContext {
        object: object_module,
        context,
        func_builder: func_ctx,
        functions: func_id_map,
        get_function,
        print_function,
    };

    comp_context.allocate_functions(&anf)?;

    comp_context.build_all(&anf)?;
   
    Ok(comp_context.object.finish())
}
