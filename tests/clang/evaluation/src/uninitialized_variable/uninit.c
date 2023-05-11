int f1(int cond) {
  int x;
  return x + cond;
}

int f2(int cond) {
  int x;
  x++;
  if (cond) {
    x = 1;
  } else {
    x = 10;
  }
  return x;
}

int f3(int cond) {
  int x;
  if (cond) {
    x = 1;
  } else {
    // x = 10;
  }
  return x;
}

int f4(int n) {
  int i;
  for (; i < n; i++) {
    return 10;
  }
  return 0;
}

int f5(int n) {
  int i;
  for (i = 0; i < n; i++) {
    return 10;
  }
  return 0;
}

int f6(int cond) {
  int x = 3;
  return x + cond;
}
