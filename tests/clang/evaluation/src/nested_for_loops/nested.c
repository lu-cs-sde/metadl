void nested(int n, int **m) {
  int i, j, k;

  for (i = 0; i < n; i++) {
    if (1) { // rando statement in between
      for (j = 0; j < n; j++) {
	switch (n) { // random statement in between
	  for (k = 0; k < n; k++) {
	    if (m[i][k] + m[k][j] < m[i][j]) {
	      m[i][j] = m[i][k] + m[k][j];
	    }
	  }
	}
      }
    }
  }
}
