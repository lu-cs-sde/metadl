#include "macros.h"

#define MY_DIV(x, y) ((x) / (y))

int x, y, z, w;

int main() {
  w = MY_DIV(x, y);
  z = MY_ADD(x, y);

  MY_PRINTF("%d %d", z, w);
}
