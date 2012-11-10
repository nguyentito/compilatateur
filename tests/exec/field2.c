
struct S { int a; int b; };

struct S s;

int main() {
  s.a = 'A';
  putchar(s.a);
  s.b = 'B';
  putchar(s.b);
  putchar(10);
}
