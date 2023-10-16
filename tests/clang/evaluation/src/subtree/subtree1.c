int foo(int x) {
  if (x > x * x) {
    return x + 1;
  } else {
    while (x) {
      return x;
    }
  }
  return 1;
}
