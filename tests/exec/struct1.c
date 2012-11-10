
struct U { char x; char y; };

int main() {
  struct U u, *p;
  int i;
  u.x = 'x';
  u.y = 'y';
  putchar(u.x);
  putchar(u.y);
  putchar(10);
  p = sbrk(5 * sizeof(struct U));
  for (i = 0; i < 5; i++) { p[i].x = 'A' + 2*i; p[i].y = 'A' + 2*i + 1; }
  for (i = 0; i < 5; i++) { putchar(p[i].x); putchar(p[i].y); } putchar(10);
  p[0] = u;
  for (i = 0; i < 5; i++) { putchar(p[i].x); putchar(p[i].y); } putchar(10);
  p[2] = u;
  for (i = 0; i < 5; i++) { putchar(p[i].x); putchar(p[i].y); } putchar(10);
  u = *(p+1);
  putchar(u.x);
  putchar(u.y);
  putchar(10);
}
