
struct S { int a; int b; };

struct S s;

int main() {
  struct S *p;
  p = &s;
  p->a = 'A';
  putchar(s.a);
  putchar(p->a);
  p->b = 'B';
  putchar(s.b);
  putchar(p->b);
  putchar(10);
}
