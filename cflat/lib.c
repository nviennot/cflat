#include <stdio.h>

#define asmlinkage __attribute__((regparm(0)))

asmlinkage int in(void) {
  int i;
  scanf("%d", &i);
  return i;
}

asmlinkage void out(int val) {
  printf("%d\n", val);
}

void *__exception_ptr;
