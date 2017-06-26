#include <stdio.h>

int i = 0, A[3] = {0, 1, 2};
void g(int x, int y) {
  x = i + 1;
  y = y + 1;
  x = i + 1;
  A[x] = A[i] + 1;
}

main() {
  g(i, A[i]);
  printf("%d %d %d %d\n", i, A[0], A[1], A[2]);
}
