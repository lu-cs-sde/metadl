int boo(int x);

int bar(int x);

int foo(int x, int y) {
  int r1 = bar(x);
  int r2 = boo(y);

  return r1 + r2;
}
