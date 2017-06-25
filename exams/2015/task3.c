#include <stdio.h>

int x = 0, s = 0;

int f(int c) {
  x = x + 1;
  return (10 * x + c);
}

int g(int a, int b) {
  if (a > 0) {
    a = a - 1;
    s = s + b + g(a, f(a)) + b + a;
  }
  return s;
}

void main() {
  int a = 3;
  int r = g(a, 0);
  printf("%d\n", r);
}
