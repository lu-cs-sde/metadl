#define NULL ((void *) 0)

int read(int *p) {
  return *p;
}

int a(void) {
  int *q = (int *) NULL;
  int *z, *w;

  w = (int *)(z = q);

  read(w);

  int x = 3, *p = &x;
  return read(p);
}
