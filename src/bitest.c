#include <stdio.h>
#include <stdlib.h>

#include <BigInt.h>

BigInt x, y, r;

void fib(BigInt n, BigInt res)
{	
	BigInt* aux1; 
	BigInt* aux2;
	aux1 = (BigInt*) calloc(1, sizeof(BigInt));
	aux2 = (BigInt*) calloc(1, sizeof(BigInt));
	(*aux1)[0] = 1;
	(*aux2)[0] = 2;
    if(BigIntLT(n, *aux2)) 
		res = n;
	else
	{ 
		BigIntSub(n, *aux1, *aux1); // n - 1
		BigIntSub(n, *aux2, *aux2); // n - 2
		fib(*aux1, *aux1);
		fib(*aux2, *aux2);
		BigIntAdd(*aux1, *aux2, res);
	} 

	free(aux1);
	free(aux2);
}

int main(int argc, char *argv[])
{
	BigIntStr xs = {'\0'}, ys = {'\0'} , rs = {'\0'};
	int base;
	
	if (argc != 2) {
		printf("Usage: bitest <base>\n");
		return 1;
	}
	
	base = atoi(argv[1]);
	switch (base) {
	case 2:
	case 8:
	case 10:
	case 16:
		break;
	default:
		printf("Error: base must be 2, 8, 10 or 16\n");
		return 1;
	}
	printf("\nEnter the value of x using base %d:\n", base);
	BigIntRead(x, base);

	// fib(x, y);
	// BigIntPrint(y, base);

	printf("\nEnter the value of y using base %d:\n", base);
	BigIntRead(y, base);

	if (BigIntEq(x, y))
		printf("\nx is equal to y");
	else if (BigIntLT(x, y))
		printf("\nx is less than y");
	else if (BigIntGT(x, y))
		printf("\nx is greater than y");
	else 
		printf("\nMy BigInt library has a bug!\n");
	
	BigIntAdd(x, y, r);
	printf("\n%s + %s = %s", BigIntToStr(x, xs, base),
	       BigIntToStr(y, ys, base), BigIntToStr(r, rs, base));

	BigIntSub(x, y, r);
	printf("\n%s - %s = %s", BigIntToStr(x, xs, base),
	       BigIntToStr(y, ys, base), BigIntToStr(r, rs, base));

	BigIntMul(x, y, r);
	printf("\n%s * %s = %s", BigIntToStr(x, xs, base),
	       BigIntToStr(y, ys, base), BigIntToStr(r, rs, base));

	BigIntDiv(x, y, r);
	printf("\n%s / %s = %s", BigIntToStr(x, xs, base),
	       BigIntToStr(y, ys, base), BigIntToStr(r, rs, base));
	
	BigIntMod(x, y, r);
	printf("\n%s mod %s = %s", BigIntToStr(x, xs, base),
	       BigIntToStr(y, ys, base), BigIntToStr(r, rs, base));

	BigIntAnd(x, y, r);
	printf("\n%s & %s = %s", BigIntToStr(x, xs, base),
	       BigIntToStr(y, ys, base), BigIntToStr(r, rs, base));
	
	BigIntOr(x, y, r);
	printf("\n%s | %s = %s", BigIntToStr(x, xs, base),
	       BigIntToStr(y, ys, base), BigIntToStr(r, rs, base));	

	BigIntXor(x, y, r);
	printf("\n%s ^ %s = %s", BigIntToStr(x, xs, base),
	       BigIntToStr(y, ys, base), BigIntToStr(r, rs, base));	

	/* Unary Operators for x*/
	BigIntAssign(r, x);
	BigIntShl(r, 10);
	printf("\n%s << 10 = %s", BigIntToStr(x, xs, base),
	       BigIntToStr(r, rs, base));

	BigIntAssign(r, x);
	BigIntShar(r, 10);
	printf("\n%s >> 10 = %s", BigIntToStr(x, xs, base),
	       BigIntToStr(r, rs, base));

	BigIntAssign(r, x);
	BigIntNeg(r);
	printf("\nx=%s, ~x=%s", BigIntToStr(x, xs, base),
	       BigIntToStr(r, rs, base));

	/* Unary Operators for y*/
	BigIntAssign(r, y);
	BigIntShl(r, 10);
	printf("\n%s << 10 = %s", BigIntToStr(y, ys, base),
	       BigIntToStr(r, rs, base));

	BigIntAssign(r, y);
	BigIntShar(r, 10);
	printf("\n%s >> 10 = %s", BigIntToStr(y, ys, base),
	       BigIntToStr(r, rs, base));

	BigIntAssign(r, y);
	BigIntNeg(r);
	printf("\nx=%s, ~x %s\n", BigIntToStr(y, ys, base),
	       BigIntToStr(r, rs, base));

	return 0;
}
