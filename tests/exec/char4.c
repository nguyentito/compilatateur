
char f(char x, char y) {
  return x+y;
}

int main() {
  char c;
  putchar(f('A', 0));
  putchar(f('A', 1));
  putchar(f('A', 2));
  c = f('A', 3);
  putchar(c);
  c++;
  putchar(c);
  putchar(10);
}
