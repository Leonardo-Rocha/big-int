        .section       .data
        
fmt:    .asciz "num: %hi\n"
base:   .asciz "base: %d\n"
BigInt: .fill 512              # n[512] = {0}

        .text
        .globl _start, BigIntRead, CharToNumber
        .extern printf
 
_start: 
        movq $BigInt,%rdi # n = BigInt
        movq $10,%rsi     # b = 10

        call BigIntRead   # BigIntRead(BigInt, 10);

        movq $60,%rax    # exit syscall
        movq $0,%rdi     # return value: exit(0)
        syscall

// void CharToNumber(int c)
// %rdi = c 
CharToNumber:
        subq    $48,%rdi       # %rdi = %rdi - 48 ; transforms char into number
        cmpq    $10,%rdi       # %edi >= $10 ;  
        jb      char_return    
        subq    $7,%rdi        #  Special conversion for hexa > 9
char_return:
        movq    %rdi,%rax
        ret

// int BigIntRead(BigInt n, int base);
// %rdi = n[]; rsi = base;      
BigIntRead:
        pushq	%rbp
        subq	$4104,%rsp	# Allocate a buffer for 4097-char string
                    # Stack must be 16-byte aligned
                    # 4097+8 = 4105, but 4105 is not
                    # multiple of 16. 4104+8=4112, which
                    # is multiple of 16.

        pushq   %rdi # push n[]
        pushq   %rsi # push base

        movq    $base,%rdi
        xorq    %rax,%rax     
        call    printf        # printf("base: %d", base);

        popq    %r9   # t9 = b
        popq    %r8    # t8 = n[]

        movl	$0,%eax		# sys_read
        movl	$0,%edi		# Standard input
        leaq	(%rsp),%rsi	# Address of the local buffer - buf[4096]
        movl	$4097,%edx	# Maximum length of the input string
        syscall                 # sys_read(stdin, buf, 4097)

        # Loop through the buffer and print each value read.
        movl    $0,%r15d        # i = 0
        movq    %rax,%r14       # t14 = size
        decq    %r14            # size--
        jmp     buffer_loop_test
buffer_loop_body:    
        xorq    %r10,%r10
        movb    (%rsp,%r15),%r10b # num = buf[i]

        movq    %r10,%rdi 
        call    CharToNumber    # converts char to number
        movl    %eax,%r10d

        # After reading the input string, you must check if
	# it is valid. For example, a base-2 number should have
	# only the digits [0-1], a base-10 number should have
	# only the digits 0-9, and so on.
        cmpl    %r9d,%r10d      # num < base    
        jb     	VALID           # in representation range
	movq    $-1, %rax       # ret = -1
        jmp     BigIntRead_return  
VALID:  
        pushq   %r8     # push n[]
        pushq   %r9     # push b

        # movq    $fmt,%rdi
        # movq    %r10,%rsi
        # xorq    %rax,%rax
        # call    printf          # printf("num: %hd", num);

        # Only for testing
        movl    %r10d,(%r8,%r15) # n[i] = num

        movq    $fmt,%rdi
        movq    (%r8,%r15),%rsi
        xorq    %rax,%rax
        call    printf          # printf("num: %hd", n[i]);

        popq    %r9   # t9 = b
        popq    %r8   # t8 = n[]

        # TODO: insert logic here

        incl    %r15d   # i++
buffer_loop_test:
        cmpq    %r14,%r15         # i < size
        jb      buffer_loop_body

BigIntRead_return:
        # deallocate the local variable and restore the stack
        # to where it was at the beginning of the function
        addq	$4104,%rsp
        popq	%rbp
        ret
