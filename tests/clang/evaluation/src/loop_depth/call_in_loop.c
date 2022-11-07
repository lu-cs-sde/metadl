void leaf(void) { }

void bar(int n) {
  for (int i = 0; i < n; ++i) {
    leaf();
  }
}

void foo(void) {
  while (1) {
    do {
      bar(5);
    } while (100);
  }
  bar(3);
}
