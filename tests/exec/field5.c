
struct S { char a; char b; };

int main() {
  struct S s;
  s.a = 'A';
  putchar(s.a);
  s.b = 'B';
  putchar(s.b);
  putchar(10);
}
