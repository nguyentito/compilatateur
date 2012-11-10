
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
  putchar(createu('x', 'y').x);
  putchar(creates('A', createu('x', 'y'), 'B').u.y);
  s.u = createu('X', 'Y');
  putchar(s.u.x);
  putchar(10);
}
