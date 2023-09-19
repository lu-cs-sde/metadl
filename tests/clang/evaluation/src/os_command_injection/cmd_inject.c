#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

//extern void *stdin;

int test1() {
  char s[100];
  fgets(s, sizeof(s) / sizeof(char), stdin);
  system(s);
}

int test2() {
  char s[100], t[100];
  fgets(s, sizeof(s) / sizeof(char), stdin);
  memcpy(t, s, sizeof(t));
  system(t);
}

int test3_helper(void *src, void *dst, int size) {
  memcpy(dst, src, size);
}

int test3() {
  char s[100], t[100];
  fgets(s, sizeof(s) / sizeof(char), stdin);
  test3_helper(s, t, sizeof(t));
  system(t);
}
