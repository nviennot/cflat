#include <stdio.h>
#include <stdlib.h>

#define asmlinkage __attribute__((regparm(0)))

asmlinkage int in(void) {
  int i;
  scanf("%d", &i);
  return i;
}

asmlinkage void out(int val) {
  printf("%d\n", val);
}

asmlinkage void __uncaught_exception(int ex) {
  printf("uncaught exception: %d\n", ex);
  exit(1);
}

void *__exception_ptr;
