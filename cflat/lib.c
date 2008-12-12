#include <stdio.h>

int in(void)
{
	int i;
	scanf("%d", &i);
	return i;
}

void out(int val)
{
	printf("%d ", val);
}

void outln(int val)
{
	printf("%d\n", val);
}

void ln()
{
	printf("\n");
}

void print(int val)
{
	printf("%d\n", val);
}
