
struct S { int a; int b; };

void f(struct S x) {
  putchar(x.a);
  putchar(x.b);
}

int main() {
  struct S s;
  s.a = 'A';
  s.b = 'B';
  f(s);
  putchar(10);
}
