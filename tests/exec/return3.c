
struct U { char x; char y; };

struct U createu(char x, char y) {
  struct U u;
  u.x = x;
  u.y = y;
  return u;
}

struct S { int a; struct U u; int b; };

struct S creates(int a, struct U u, int b) {
  struct S s;
  s.a = a;
  s.u = u;
  s.b = b;
  return s;
}

int main() {
  struct U u;
  struct S s;
  u = createu('x', 'y');
  s = creates('A', u, 'B');
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
