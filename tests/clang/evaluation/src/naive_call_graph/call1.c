void f2(void);

void f3(void) {
  f2();
}

void f1(void) {
  f2();
}

void f2(void) {
  for (int i = 0; i < 100; ++i) {
    f3();
  }
}
