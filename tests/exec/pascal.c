
/* triangle de Pascal modulo 7 */

void print_row(int *r, int i) {
  int j;
  for (j = 0; j <= i; j++)
    if (r[j] != 0)
      putchar('*');
    else
      putchar('.');
  putchar(10);
}

void compute_row(int *r, int i) {
  int j;
  for (j = i; j > 0; j--)
    r[j] = (r[j] + r[j-1]) % 7;
  r[0] = 1;
}

void pascal(int n) {
  int i, *r;
  r = sbrk((n+1) * sizeof(int));
  for (i = 0; i < n; i++) {
    r[i] = 0;
    compute_row(r, i);
    print_row(r, i);
  }
}

int main() {
  pascal(42);
  return 0;
}
