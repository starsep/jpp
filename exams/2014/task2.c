#include <stdio.h>

int x = 10;
int s = 0;

int f(int a) {
  s += x;
  if (a > 0) {
    a = a - 1;
    s += f(a);
  }
  return a;
}

void g(int a) {
        int x = 3;
        s += f(x);
        s += x;

}

void main() {
    g(x);
    printf("%d\n", s);
}
