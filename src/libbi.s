// Use tab 8 for better formating		

	.text
	.globl BigIntRead, BigIntPrint, BigIntToStr
	.globl BigIntEq, BigIntLT, BigIntGT
	.globl BigIntAssign, BigIntAdd, BigIntSub
	.globl BigIntMul, BigIntDiv, BigIntMod
	.globl BigIntAnd, BigIntOr, BigIntXor
	.globl BigIntShl, BigIntShar, BigIntNeg
	
// Read a BigInt n in the base b.		
// The base can be 2, 8, 10 or 16.		
// int BigIntRead(BigInt n, int b);		
// Must create a local buffer for the input string.
BigIntRead:
	pushq	%rbp
	
	subq	$4104,%rsp	# Allocate a buffer for 4097-char string
				# Stack must be 16-byte aligned
				# 4097+8 = 4105, but 4105 is not
				# multiple of 16. 4104+8=4112, which
				# is multiple of 16.

	addl    $4104,%rsi     # so we can go to the start 
    movq    %rdi,%rcx      # %rcx = n[]
    movl    %esi,%e9       # %e9 = b
    movl    $0,%eax        # i = 0
# Memory is indexed in bytes so we must iterate 
# TODO : TRATAR CALEE SAVE DOS %r12, %r11
LB1:
    incl    %eax;               # i++
    subl    $48,%rdx;           # %rdx = %rdx - 48 ; transforms chat into number
    cmpl    %edx,$10;           # %edx >= $10 ;  
    jl      NOT_HEXA;           # Special conversion for hexa>9
    subq    $7,%edx;            # %edx -= 7 ;
NOT_HEXA:
    cmpl    %edx,%e9            # %edx < %e9    
    jl     	VALID             	# in representation range
	movl    $-1, %eax         # ret = -1
# NAIVE APPROACH
VALID:
    testl   %e9,$2              # e9 == 2 
    je      BYNARY
    testl   %e9,$8              # e9 == 8
    je      OCTAL
    testl   %e9,$10             # e9 == 10
    je      DECIMAL
HEXA:
    sall    $28,%edx            # edx << 28; to insert in the most significant bits
    sarl    $4,%e10;            # e10 >> 4 ; shifting right so it can fit the next hexa    
    addl    %edx,%e10           # accumulate in the 32 bit

    testl   %e12,$8             # if e12 != 8
    jne     NO_MEM_STORE;       # do not store de value in the BigInt, for it is not ready yet 
    movl    %e10,(%rcx)         # n[k] = value
    movl    $0,%e12             # e12 = 0 ; flush the counter
    jmp     NO_MEM_STORE;
BYNARY:
    sall    $31,%edx            # edx << 31; to insert in the most significant bit
    sarl    $1,%e10;            # edx >> 1 ; shifting right so it can fit the next bit    
    addl    %edx,%e10           # accumulate in the 32 bit

    testl   %e12,$10            # if e12 != 32
    jne     NO_MEM_STORE;       # do not store de value in the BigInt, for it is not ready yet 
    movl    %e10,(%rcx)         # n[k] = value
    movl    $0,%e12             # e12 = 0 ; flush the counter
    jmp     NO_MEM_STORE;
OCTAL:
    sall    $26,%edx;           # edx << 29; to insert in the most significant bits
    sarl    $3,%e10;            # edx >> 3 ; shifting right so it can fit the next octal    
    addl    %edx,%e10           # accumulate in the 32 bit

    testl   %e12,$10            # if e12 != 10
    jne     NO_MEM_STORE;       # do not store de value in the BigInt, for it is not ready yet 
    movl    %e10,(%rcx)         # n[k] = value
    movl    $0,%e12             # e12 = 0 ; flush the counter
    jmp     NO_MEM_STORE;
DECIMAL:
    sall    $4,%edx;           # edx << 4; to insert in the most significant bits
    sarl    $1,%e10;            # edx >> 1 ; shifting right so it can fit the next decimal
    movl    %edx,%e11;    
    sall    $2,%e11;            # e11 << 2 ; so we can multiply by 4
    addl    %e11,%edx;
    addl    %edx,%e10;           # accumulate in the 32 bit

    testl   %e12,$5;            # if e12 != 5
    jne     NO_MEM_STORE;       # do not store de value in the BigInt, for it is not ready yet 
    movl    %e10,(%rcx)         # n[k] = value
    movl    $0,%e12             # e12 = 0 ; flush the counter
NO_MEM_STORE:    
    incl    %e12                # e12++ ; counts another shift
    addl    $4,%ecx             # k = k + 4
LC1:
    leaq    (%rsi,%rax),%rdx    # %rdx = v[i]
    cmpl    %rdx,$0             # if v[i] != '\0'
    jne     LB1;                # return to loop



	movl	$0,%eax		# sys_read
	movl	$0,%edi		# Standard input
	leaq	(%rsp),%rsi	# Address of the local buffer
	movl	$4096,%edx	# Maximum length of the input string
	syscall

	# After reading the input string, you must check if
	# it is valid. For example, a base-2 number should have
	# only the digits [0-1], a base-10 number should have
	# only the digits 0-9, and so on.


	# deallocate the local variable and restore the stack
	# to where it was at the beginning of the function
	addq	$4104,%rsp
	popq	%rbp
	ret
	
// Print a BigInt n in the base b.		
// The base can be 2, 8, 10 or 16.		
// int BigIntPrint(BigInt n, int b);		
BigIntPrint:
	ret
	
// Convert n to string in the base b.		
// The base can be 2, 8, 10 or 16.		
// char *BigIntToStr(BigInt n, char *buf, int b); 
BigIntToStr: 
	ret

// BigIntEq: returns x == y	
// int BigIntEq(BigInt x, BigInt y);
BigIntEq:
	xorq	%rax,%rax
	ret

// BigIntLT: returns x < y	
// int BigIntLT(BigInt x, BigInt y);
BigIntLT:
	xorq	%rax,%rax
	ret
	
// BigIntGT: returns x > y	
// int BigIntGT(BigInt x, BigInt y);
BigIntGT:
	xorq	%rax,%rax
	ret
	
// BigIntAssign: x = y				
// void BigIntAssign(BigInt x, BigInt y);	
BigIntAssign:
	ret
	
// BigIntAdd: xpy = x + y				
// void BigIntAdd(BigInt x, BigInt y, BigInt xpy);	
BigIntAdd:
	ret
	
// BigIntSub: xmy = x - y	
// void BigIntSub(BigInt x, BigInt y, BigInt xmy);	
BigIntSub:
	ret
	
// BigIntMul: xty = x * y	
// void BigIntMul(BigInt x, BigInt y, BigInt xty);
BigIntMul:
	ret
	
// BigIntDiv: xdy = x / y	
// void BigIntDiv(BigInt x, BigInt y, BigInt xdy);
BigIntDiv:
	ret
	
// BigIntMod: xmy = x % y	
// void BigIntMod(BigInt x, BigInt y, BigInt xmy);
BigIntMod:
	ret
	
// BigIntAnd: xay = x & y	
// void BigIntAnd(BigInt x, BigInt y, BigInt xay);
BigIntAnd:
	ret
	
// BigIntOr: xoy = x | y	
// void BigIntOr(BigInt x, BigInt y, BigInt xoy);
BigIntOr:
	ret
	
// BigIntXor: xxy = x ^ y	
// void BigIntXor(BigInt x, BigInt y, BigInt xxy);
BigIntXor:
	ret
	
// BigIntShl: x = x << n	
// void BigIntShl(BigInt x, int n);
BigIntShl:
	ret
	
// BigIntShar: x = x >> n	
// void BigIntShar(BigInt x, int n);
BigIntShar:
	ret
	
// BigIntNeg: x = ~x		
// void BigIntNeg(BigInt x);
BigIntNeg:
	ret
