int variadic1(int x, ...) { return x; }

int variadic2(int y, ...);

int foo() {
  return 0;
}

int bar(int x, int y) {
  return x + y;
}

int main() {

  variadic1(1, 2);
  foo();
  bar(1, 2);

  {
    variadic1(1, 2);
  }

}
