
struct S { char a; char b; };

int main() {
  struct S *p;
  p = sbrk(sizeof(struct S));
  p->a = 'A';
  putchar(p->a);
  p->b = 'B';
  putchar(p->b);
  putchar(10);
}
