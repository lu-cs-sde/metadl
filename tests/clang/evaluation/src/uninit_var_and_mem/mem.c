int test1_pos() {
  int x[10];
  return x[3];
}


int test2_pos() {
  int *p;
  p = (int *) malloc(sizeof(int));
  return *p;
}

int test3_pos() {
  int *p = (int *) malloc(sizeof(int));
  return *p;
}

int test4_neg() { // false negative
  int x[10];
  x[2] = 3;
  return x[5];
}

int test5_neg() { // false negative
  int *x = malloc(10 * sizeof(int));
  x[2] = 3;
  return x[5];
}

int test6_neg(int cond) { // false negative
  int x[10];

  if (cond) {
    x[1] = 5;
  }

  return x[0];
}

int test7_pos(void) {
  int x[10], y;
  return x[2];
}

int test8_pos(void) {
  int x[10], y;
  y = x[3];
  return y;
}

struct S {
  int a, b;
};

int test9_pos(void) {
  struct S *p;
  struct S s[10];

  p = s;

  return p[3].a + p[3].b;
}

int test10_neg(void) {
  struct S *p;
  struct S s[10];

  p = s;

  for (int i = 0; i < 10; ++i) {
    p[i].a = 0;
    p[i].b = 0;
  }

  return p[2].a + p[3].b;
}
