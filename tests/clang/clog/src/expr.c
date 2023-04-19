int foo(int x, int y) {
  int z;
  z = y;

  int (*p)(int, int);

  p = foo;

  return z;
}
