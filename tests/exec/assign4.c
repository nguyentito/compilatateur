
struct S { int a; int b; };

void f(struct S x) {
  putchar(x.a);
  putchar(x.b);
}

int main() {
  struct S s, u;
  s.a = 'A';
  s.b = 'B';
  f(s);
  u.a = 'C';
  u.b = 'D';
  f(u);
  f(s = u);
  putchar(10);
}
