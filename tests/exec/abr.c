/* arbres binaires de recherche */

struct ABR {
  int valeur;
  struct ABR *gauche, *droite;
};

struct ABR* make(int v, struct ABR *g, struct ABR *d) {
  struct ABR * s;
  s = sbrk(sizeof(struct ABR));
  s->valeur = v;
  s->gauche = g;
  s->droite = d;
  return s;
}

void insere(struct ABR *a, int x) {
  if (x == a->valeur) return;
  if (x < a->valeur) {
    if (a->gauche == 0)
      a->gauche = make(x, 0, 0);
    else
      insere(a->gauche, x);
  } else
    if (a->droite == 0)
      a->droite = make(x, 0, 0);
    else
      insere(a->droite, x);
}

int contient(struct ABR *a, int x) {
  if (x == a->valeur) return 1;
  if (x < a->valeur && a->gauche != 0) return contient(a->gauche, x);
  if (a->droite != 0) return contient(a->droite, x);
  return 0;
}

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

char* itoa(int n) {
  int len, abs;
  char *s;
  len = 1;
  if (n < 0) { abs = -n; len++; } else abs = n;
  abs = abs / 10;
  while (abs != 0) { len++; abs = abs / 10; }
  s = sbrk(len+1);
  s[len--] = 0;
  if (n < 0) { s[0] = '-'; n = -n; };
  while (n > 9) { s[len--] = '0' + n % 10; n = n / 10; }
  s[len] = '0' + n % 10;
  return s;
}

char* to_string(struct ABR *a) {
  char *s;
  s = "(";
  if (a->gauche != 0) s = append(s, to_string(a->gauche));
  s = append(s, itoa(a->valeur));
  if (a->droite != 0) s = append(s, to_string(a->droite));
  return append(s, ")");
}

int main() {
  struct ABR *dico;
  dico = make(1, 0, 0);
  insere(dico, 17);
  insere(dico, 5);
  insere(dico, 8);
  print_string(to_string(dico));
  putchar(10);
  if (contient(dico, 5) && !contient(dico, 0) &&
      contient(dico, 17) && !contient(dico, 3))
    print_string("ok\n");
  insere(dico, 42);
  insere(dico, 1000);
  insere(dico, 0);
  print_string(to_string(dico));
  putchar(10);
}

