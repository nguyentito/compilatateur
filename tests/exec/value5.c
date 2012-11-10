
struct S { int a; int b; };

void f(struct S x) {
  putchar(x.a);
  putchar(x.b);
  x.a = 'C';
  putchar(x.a);
}

struct S s;

int main() {
  s.a = 'A';
  s.b = 'B';
  f(s);
  putchar(s.a); /* on vérifie qu'il y avait bien eu passage par valeur */
  putchar(10);
}
