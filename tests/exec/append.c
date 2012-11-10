
int strlen(char *s) {
  int len;
  len = 0;
  while (*s++) len++;
  return len;
}

char* append(char *s1, char *s2) {
  char * s;
  int len;
  char c;
  len = strlen(s1) + strlen(s2);
  s = sbrk(len + 1);
  len = 0;
  while (c = *s1++) s[len++] = c;
  while (c = *s2++) s[len++] = c;
  s[len] = 0;
  return s;
}

void print_string(char *s) {
  char c;
  while (c = *s++) putchar(c);
}

void print_endline(char *s) {
  print_string(s);
  putchar(10);
}

int main() {
  print_endline(append("foo", "bar"));
  print_endline(append("hello", " world"));
  print_endline(append("il semble ", append("que cela ", "fonctionne")));
  return 0;
}
