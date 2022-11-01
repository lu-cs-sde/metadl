int f(int n) {
  for (int i = 0; i < 3; i++) {
    int j = i;
    for (; j < 5; ++j) {
      n = n + 1;
    }
  }


  return n;
}
