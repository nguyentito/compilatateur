
struct S { char a; char b; };

int main() {
  struct S *p;
  p = sbrk(5 * sizeof(struct S));
  p->a = 'A';
  p->b = 'B';
  p++;
  p->a = 'C';
  p->b = 'D';
  p++;
  p->a = 'E';
  p->b = 'F';
  putchar(p->a);
  putchar(p->b);
  --p;
  putchar(p->a);
  putchar(p->b);
  --p;
  putchar(p->a);
  putchar(p->b);
  putchar(10);
}
