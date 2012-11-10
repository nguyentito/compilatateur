
int main() {
  char *s;
  int i;
  s = sbrk(11);
  s[0] = 'h';
  s[1] = 'e';
  s[2] = 'l';
  s[3] = 'l';
  s[4] = 'o';
  s[5] = ' ';
  s[6] = 'w';
  s[7] = 'o';
  s[8] = 'r';
  s[9] = 'l';
  s[10] = 'd';
  for (i = 0; i < 11; i++) putchar(s[i]);
  putchar(10);
}
