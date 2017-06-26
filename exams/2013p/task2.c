#include <stdio.h>

int g = 7;

int f(int x, int y) {
    x = y + x;
    x += g;
    return x + y;
}

int k(int c) {
    return g+c;
}

void main() {
    int g = 5;
    int n = f(g, k(g));
    printf("%d %d\n", g, n);
}
