#include <stdlib.h>

int* id(int *p) {
  return p;
}

int* foo(int *p) {
  int *q;
  q = p;
  return q;
}

int main(int argc, char **argv) {
  int *p1 = (int*)malloc(sizeof(int));
  int *q;
  int **r = &q;
  int *tmp;
  tmp = id(p1);
  *r = tmp;
  int *s = *r;
  foo(s);
}
