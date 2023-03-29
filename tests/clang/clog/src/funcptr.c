int (*f)(int);

int (*g)(int (*q)(int, int));

int (*k)(int (*h)(int));

int (*(*m)(int))(int);

typedef int (*INT_TO_INT)(int);
typedef INT_TO_INT (*INT_TO_INT_TO_INT)(int);

INT_TO_INT tf;
INT_TO_INT_TO_INT tg;

// array of function pointers
int (*a1[10])(int);
INT_TO_INT a2[100];


int test() {
  int x = a1[3](2);
  return x;
}
