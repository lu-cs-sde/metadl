#include <iterator>
extern "C" {
  short** matrix_alloc(int m, int n);
}


namespace {
template<int FROM, int TO>
class Range {
  static_assert  (FROM < TO, "");

public:
  class iterator : public std::iterator<std::input_iterator_tag, int, int, const int*, int> {

    long i = FROM;
  public:
    iterator(int n) : i(n) { }
    iterator& operator++() {
      i++;
      return *this;
    }

    iterator operator++(int) {
      iterator ret = *this;
      i++;
      return ret;
    }

    bool operator==(const iterator &rhs) const { return this->i == rhs.i; }
    bool operator!=(const iterator &rhs) const { return *this != rhs; }
    reference operator*() const { return i; }
  };

public:

  iterator begin() { return iterator(FROM); }
  iterator end() { return iterator(TO-1); }
};

class Klass {
public:
  static void matrix_alloc_x10(int m, int n) {
    for (int i : Range<0, 10>()) {
      matrix_alloc(3, 4);
    }

    Range<0, 10> r;
    for (Range<0, 10>::iterator it = r.begin(); it != r.end(); ++it) {
      matrix_alloc(3, 4);
    }
  }
};

static void matrix_alloc_x10(int m, int n) {
  for (int i : Range<0, 10>()) {
    matrix_alloc(3, 4);
  }

  Range<0, 10> r;
  for (Range<0, 10>::iterator it = r.begin(); it != r.end(); ++it) {
    matrix_alloc(3, 4);
  }
}

static void matrix_alloc_x100(int m, int n) {
  Range<0, 10> r;
  for (Range<0, 10>::iterator it = r.begin(); it != r.end(); ++it) {
    matrix_alloc_x10(3, 4);
  }
}


}
