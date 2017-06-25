#include <stdio.h>

int x = 0, s = 0;

int f(int c) {
    x = x + 100;
    return (x + c);
}

int g(int a, int b) {
    int x = a;
    if(x < 3) {
        a = a + 1;
        s = s + b + g(a, f(a)) + b + a;
    }
    return s;
}

void main() { int a = 1; int r = g(a, 0); printf("%d\n", r); }
