int test1_pos(void) {
  int x;
  return x;
}

int test2_neg(void) {
  int x = 3;
  return x;
}

int test3_neg(void) {
  int x;
  x = 3;
  return x;
}

int test4_neg(int cond) {
  int x;
  while (cond) {
    x = 10;
  }
  return x;
}

int test5_pos(void) {
  int x;
  x = x;
  return x;
}

int test6_neg(void) {
  int x;
  int y = 5;
  x = y;
  return x;
}
