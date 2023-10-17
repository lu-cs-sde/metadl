#include <stdlib.h>

int foo(int *p, int *q, int n) {
  for (int i = 0; i < n; ++i) {
    *p = 0;
  }
  return 1;
}

/* The checker should be able to detect that p is not NULL when calling foo,
   and it shouldn't produce any warning. */
int bar(int *p, int *q) {
  int cnt = 0;
  if (p != NULL) {
    cnt += foo(p, q, 10);
  }
  return 0;
}

int entry(void) {
  bar (NULL, NULL);
}
