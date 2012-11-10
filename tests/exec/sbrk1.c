
int main() {
  char *s;
  s = sbrk(11);
  s[0] = 'h';
  putchar(s[0]);
  putchar(10);
}
