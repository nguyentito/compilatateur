
struct S { char a; char b; };

struct S s;

int main() {
  s.a = 'A';
  putchar(s.a);
  s.b = 'B';
  putchar(s.b);
  putchar(10);
}
