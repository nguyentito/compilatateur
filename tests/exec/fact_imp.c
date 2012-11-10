int fact_imp(int n) {
  int res;
  for (res = 1 ; n > 1; )
    res = res * n--;
  return res;
}

int main() {
  if (fact_imp(0) == 1) putchar('1');
  if (fact_imp(1) == 1) putchar('2');
  if (fact_imp(5) == 120) putchar('3');
  putchar(10);
}
