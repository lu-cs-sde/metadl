#include <stdlib.h>

typedef struct arena arena;
// allocate from the arena
extern void *aalloc(arena *, size_t);
// free the whole arena
extern void areset(arena *);

void do_something(int *, int *);

void entry0(arena *ma) {
  int *p = aalloc(ma, sizeof(int));
  int *q = malloc(sizeof(int));
  // do work
  free(p);
  free(q);
}

void cleanup1(int *x, int *y) {
  free(x);
  free(y);
}
void entry1(arena *ma) {
  int *p = aalloc(ma, sizeof(int));
  int *q = malloc(sizeof(int));
  // do work
  cleanup1(p, q);
}

int * alloc21(arena *ma) {
  int *p = aalloc(ma, sizeof(int));
  return p;
}
int * alloc22(arena *ma) {
  int *p = malloc(sizeof(int));
  return p;
}
void entry2(arena *ma) {
  int *p = alloc21(ma);
  int *q = alloc22(ma);
  // do work
  free(p);
  free(q);
}
