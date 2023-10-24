void f(void) {
 label:
  if (1)
    goto label;
 }

int g(int cond) {
  int exit_code = 0;

  if (cond) {
    exit_code = 1;
    goto error_exit;
  }
  return 0;

 error_exit:
  return exit_code;
}

int h(int cond) {
  if (cond)
    goto normal_exit;
  else
    goto error_exit;
 normal_exit:
  return 0;
 error_exit:
  return 1;

}
