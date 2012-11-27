
int strlen(char *s) {
  int len;
  len = 0;
  while (*s++) len++;
  return len;
}

void print_int(int n) {
  if (n > 9) print_int(n / 10);
  putchar('0' + n%10);
}

int main() {
  print_int(strlen(""));
  putchar(10);
  print_int(strlen("hello world"));
  putchar(10);
  print_int(strlen("hello world\x0a"));
  putchar(10);
  print_int(strlen("foo bar"));
  putchar(10);
  print_int(strlen("answer to the question of life, the uni..."));
  putchar(10);
  return 0;
}
