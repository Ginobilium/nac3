// clang -Wall -o demo demo.c mandelbrot.o

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
            // printf("ERROR\n");
            printf("%d", x);
        }
    }
    return 0;
}

extern int run();

int main() {
    run();
    return 0;
}
