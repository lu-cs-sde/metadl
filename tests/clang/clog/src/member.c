struct Point {
  int x;
  int y;
};

struct Line {
  struct Point p0, p1;
};

struct LineP {
  struct Point *p0, *p1;
};

int pow2(int x) {
  return x * x;
}

float length2(struct Line l) {
  return pow2(l.p0.x -
	      l.p1.x) +
    pow2(l.p0.y -
	 l.p1.y);
}

float length2p(struct LineP l) {
  return pow2(l.p0->x -
	      l.p1->x) +
    pow2(l.p0->y -
	 l.p1->y);
}
