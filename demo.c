// gcc -Wall -o demo demo.c test.o

#include <stdio.h>
#include <string.h>

int output(int x) {
    static char chars[] = " .,-:;i+hHM$*#@  ";
    if(x < 0) {
        putchar('\n');
    } else {
        if(x < strlen(chars)) {
            putchar(chars[x]);
        } else {
            printf("ERROR\n");
        }
    }
    return 0;
}

extern int run();

int main() {
    run();
    return 0;
}
