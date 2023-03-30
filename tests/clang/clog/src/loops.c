int f1(int m, int n) {
  while (m) {
    while (n)
      n--;
    m--;
  }

  do {
    // nothing
  } while (0);

  do ; while (0);


  for (int i = 0; i < 3; i++) {
    // do nothing
    for (i = 0; i < 3; i++) ;
  }

  for (;;) {
    // empty for loop
  }

  for (int i = 0; ; ) {
    // empty
  }

  for (m = 0; ; ) {
    // empty
  }

  for (; m < n; ) {
    // empty
  }

  for (; ; n--) {
    // empty
  }

  return m;
}
