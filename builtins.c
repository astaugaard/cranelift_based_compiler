

#include <stdio.h>
void print_num(int num) {
    printf("%d\n",num);
}

int get_num() {
    int value = 0;
    int result = scanf("%d",&value);

    if (result == EOF) {
        return 0;
    }

    if (result == 0) {
        while (fgetc(stdin) != '\n');
    }

    return value;
}
