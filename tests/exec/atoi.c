
int atoi(char *s) {
  int v;
  char c;
  v = 0;
  while (c = *s++)
    v = 10 * v + c-'0';
  return v;
}

void test(int n, char *s) {
  if (n == atoi(s)) putchar('1'); else putchar('0');
}

int main() {
  test(0, "0");
  test(1, "1");
  test(42, "42");
  test(0xff, "255");
  test(077, "63");
  putchar(10);
  return 0;
}
