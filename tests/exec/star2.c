
struct U { char x; char y; };

struct S { int a; struct U u; int b; };

struct U u, *p;
struct S s;

int main() {
  u.x = 'x';
  u.y = 'y';
  putchar(u.x);
  putchar(u.y);
  putchar(10);
  p = &u;
  putchar(p->x);
  putchar(p->y);
  putchar(10);
  s.u.x = 'X';
  s.u.y = 'Y';
  p = &s.u;
  putchar(p->x);
  putchar(p->y);
  putchar(10);
  *p = u;
  putchar(p->x);
  putchar(p->y);
  putchar(10);
  p = &u;
  p->x = 'a';
  p->y = 'b';
  s.u = *p;
  putchar(s.u.x);
  putchar(s.u.y);
  putchar(10);
}
