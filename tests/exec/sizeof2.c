
struct S { int a; int b; };
struct T { int a; char b; char c; char* p; };
struct U { char a; char b; };

int main() {
  putchar(65 + sizeof(struct S));
  putchar(10);
  putchar(65 + sizeof(struct T));
  putchar(10);
  putchar(65 + sizeof(struct U));
  putchar(10);
  putchar(65 + sizeof(struct U *));
  putchar(10);
}
