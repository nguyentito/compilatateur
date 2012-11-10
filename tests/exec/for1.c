int main() {
  int i;
  int cpt;
  cpt = 0;
  for(i = 0; i < 10; i++) {
    int j;
    for (j = 10; j > 0; j--)
      ++cpt;
  }
  if (cpt == 100)
    putchar('!');
  putchar(10);
}
