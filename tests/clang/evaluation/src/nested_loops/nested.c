void nested(int n, int **m) {
  while (n) {
    for (int i = 0; i < n; i++) {
      do
	; // nothing;
      while(n);
    }
  }
}
