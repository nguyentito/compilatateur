
void print_string(char *s) {
  int i;
  char c;
  for (i = 0; c = s[i]; i++)
    putchar(c);
}

void print_endline(char *s) {
  print_string(s);
  putchar(10);
}

int main() {
  print_endline("foo");
  print_string("hello world\n");
}
