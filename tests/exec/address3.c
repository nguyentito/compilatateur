
int x;
int *p;

int main() {
  p = &x;
  *p = 'A';
  putchar(x);
  putchar(*p);
  x = 'B';
  putchar(x);
  putchar(*p);
  *p = 'C';
  putchar(x);
  putchar(*p);
  putchar(10);
}
