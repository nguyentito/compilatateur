
struct U { char x; char y; };

struct S { int a; struct U u; int b; };

int main() {
  struct U u;
  struct S s;
  s.a = 'A';
  s.b = 'B';
  s.u.x = 'x';
  s.u.y = 'y';
  putchar(s.a);
  putchar(s.u.x);
  putchar(s.u.y);
  putchar(s.b);
  putchar(10);
  u.x = 'X';
  u.y = 'Y';
  putchar(s.a);
  putchar(s.u.x);
  putchar(s.u.y);
  putchar(s.b);
  putchar(10);
  s.u = u;
  putchar(s.a);
  putchar(s.u.x);
  putchar(s.u.y);
  putchar(s.b);
  putchar(10);
}
