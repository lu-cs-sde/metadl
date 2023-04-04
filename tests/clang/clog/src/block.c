int foo() {
  // empty blocks
  {}
  {}

  {
    return 1;
  }

  {
    return 2;
    return 3;
  }

  {
    return 4;
    return 5;
    return 6;
  }
}
