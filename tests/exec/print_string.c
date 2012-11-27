
void print_string(char *s) {
  char c;
  while (c = *s++) putchar(c);
}

void print_endline(char *s) {
  print_string(s);
  putchar(10);
}

int main() {
  print_endline("foo");
  print_string("hello world\x0a");
}
