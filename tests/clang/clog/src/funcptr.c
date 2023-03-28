int (*f)(int);

int (*g)(int (*q)(int, int));

int (*k)(int (*h)(int));

int (*(*m)(int))(int);

typedef int (*INT_TO_INT)(int);
typedef INT_TO_INT (*INT_TO_INT_TO_INT)(int);

 INT_TO_INT tf;
INT_TO_INT_TO_INT tg;
