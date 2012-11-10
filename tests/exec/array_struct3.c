
struct S { int a; char b; int c; };

int main() {
  struct S *p;
  p = sbrk(5 * sizeof(struct S));
  p[0].a = 'A';
  p[0].b = 'B';
  putchar(p[0].a);
  putchar(p[0].b);
  p[1].a = 'a';
  p[1].b = 'b';
  putchar(p[0].a);
  putchar(p[0].b);
  putchar(p[1].a);
  putchar(p[1].b);
  p[2].a = 'C';
  putchar(p[2].a);
  p[3].b = 'D';
  putchar(p[3].b);
  p[4].b = 'E';
  putchar(p[4].b);
  putchar(10);
}
