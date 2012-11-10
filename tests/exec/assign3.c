
struct S { int a; int b; };

void f(struct S s1, struct S s2) {
  s1.a = 'A';
  s1.b = 'B';
  putchar(s1.a);
  putchar(s1.b);
  s2 = s1;
  putchar(s2.a);
  putchar(s2.b);
  s1.a = 'C';
  s1.b = 'D';
  putchar(s1.a);
  putchar(s1.b);
  putchar(s2.a);
  putchar(s2.b);
  s1 = s2;
  putchar(s1.a);
  putchar(s1.b);
  putchar(s2.a);
  putchar(s2.b);
}

int main() {
  struct S a, b;
  f(a, b);
  putchar(10);
}
