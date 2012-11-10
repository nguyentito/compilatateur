
struct list {
int val;
struct list * next;
};

struct list * nil() {
return 0;
}


struct list * cons(int x, struct list * lst)
{
struct list * head;
head = malloc(sizeof(struct list));
head->next = lst;
head->val = x;
return head;
}

void delete(struct list *lst)
{
struct list * tmp;
while (lst != 0) {
tmp = lst->next;
free(lst);
lst = tmp;
};

}
