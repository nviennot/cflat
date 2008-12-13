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

asmlinkage void __reverse_args(int n)
{
  int i;
  for (i = 0; i < n/2; i++) {
    int tmp = (&n+1)[i];
    (&n+1)[i] = (&n+1)[n-i-1];
    (&n+1)[n-i-1] = tmp;
  }
}

void *exception_ptr;
