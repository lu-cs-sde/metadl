void foo(int n) {
  if (n) {
    return;
  }
  foo(n - 1);
}

int sum(int a, int b) {
  return a + b;
}
