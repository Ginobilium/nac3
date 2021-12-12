#include <stdio.h>
#include <string.h>

void output_int(int x) {
    printf("%d\n", x);
}

void output_long(long x) {
    printf("%ld\n", x);
}

void output_asciiart(int x) {
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
}

extern int run();

int main() {
    run();
    return 0;
}
