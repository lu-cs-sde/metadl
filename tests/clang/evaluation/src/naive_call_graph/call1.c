void f2();

void f3() {
  f2();
}

void f1() {
  f2();
}

void f2() {
  for (int i = 0; i < 100; ++i) {
    f3();
  }
}
