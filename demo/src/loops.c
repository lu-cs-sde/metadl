#import <stdlib.h>

extern void duff(register short *to, register short *from, register int count);

void matrix_copy(short **to, short **from, int m, int n) {
  for (int i = 0; i < m; ++i) {
    duff(to[i], from[i], n);
  }
}

short** matrix_alloc(int m, int n) {
  short **ret = malloc(m * sizeof(short*));
  for (int i = 0; i < m; ++i) {
    ret[i] = malloc(n * sizeof(short));
  }
  return ret;
}

void matrix_init(short **mat, int m, int n, short v) {
  for (int i = 0; i < m; ++i) {
    for (int j = 0; j < n; ++j) {
      mat[i][j] = v;
    }
  }
}

void matrix_free(short **mat, int m) {
  for (int i = 0; i < m; ++i) {
    free(mat[i]);
  }
  free(mat);
}


void f3(short **mat, int m, int n) {
  for (int i = 0; i < 100; ++i) {
      for (int i = 0; i < 100; ++i) {
	  for (int i = 0; i < 100; ++i) {
	    matrix_init(mat, m, n, 0);
	  }
      }
  }
}

void f2(short **mat, int m, int n) {
  f3(mat, m, n);
}

void f1(short **mat, int m, int n) {
  f2(mat, m, n);
}

int main(int argc, char **argv) {
  short **m1 = matrix_alloc(4, 3);
  short **m2 = matrix_alloc(4, 3);

  matrix_init(m1, 4, 3, 5);
  matrix_copy(m2, m1, 4, 3);

  int ret = m2[1][1];

  matrix_free(m1, 4);
  matrix_free(m2, 4);

  return ret;
}
