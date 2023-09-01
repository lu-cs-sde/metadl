int const2(void);

int foo(int x, int y) {
  return x + y;
}

int bar () {
  int a, b;
  a = 3;
  b = 5;

  int c;

  c = foo(a, b);

  int d = foo(3, const2());

  return c + d;
}


unsigned fib(unsigned n) {
  if (n == 0 || n == 1)
    return 1;
  return fib(n - 1) + fib(n - 2);
}

int const2(void) {
  return 2;
}
