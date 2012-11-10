/*** listes circulaires doublement chaînées ***/

struct L {
  int valeur;
  struct L *suivant, *precedent;
};

/* liste réduite à un élément */
struct L* make(int v) {
  struct L* r;
  r = sbrk(sizeof(struct L));
  r->valeur = v;
  r->suivant = r->precedent = r;
  return r;
}

/* insertion après un élément donnée */
void inserer_apres(struct L *l, int v) {
  struct L *e;
  e = make(v);
  e->suivant = l->suivant;
  l->suivant = e;
  e->suivant->precedent = e;
  e->precedent = l;
}

/* suppression d'un élément donné */
void supprimer(struct L *l) {
  l->precedent->suivant = l->suivant;
  l->suivant->precedent = l->precedent;
}

/* affichage */
void afficher(struct L *l) {
  struct L *p;
  p = l;
  putchar(p->valeur);
  for (p = p->suivant; p != l; p = p->suivant)
    putchar(p->valeur);
  putchar(10);
}

int main() {
  struct L *l;
  l = make(65);
  afficher(l);
  inserer_apres(l, 66);
  afficher(l);
  inserer_apres(l, 67);
  afficher(l);
  supprimer(l->suivant);
  afficher(l);
}
