
struct S { int a; int b; };

void g(struct S x) {
  putchar(x.a);
  putchar(x.b);
}

void f(struct S x) {
  g(x);
  x.a = 'C';
  g(x);
}

int main() {
  struct S s;
  s.a = 'A';
  s.b = 'B';
  g(s);
  f(s);
  g(s);
  putchar(10);
}
