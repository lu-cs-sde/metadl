void leaf(void) { }

void bar(int n) {
  for (int i = 0; i < n; ++i) {
    leaf(); // loop depth 2 when called from line 12
  }
}

void foo(void) {
  while (1) {
    do {
      bar(5);
      // loop depth 1
    } while (100);
    // loop depth 0 - top-level loop
  }
  bar(3);
}
