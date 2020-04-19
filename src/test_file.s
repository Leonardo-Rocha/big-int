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
// %rdi = n; rsi = base;      
BigIntRead:
        pushq	%rbp
        subq	$4104,%rsp	# Allocate a buffer for 4097-char string
                    # Stack must be 16-byte aligned
                    # 4097+8 = 4105, but 4105 is not
                    # multiple of 16. 4104+8=4112, which
                    # is multiple of 16.

        # TODO: CRIAR VARIAVEIS PARA ESSES DOIS TEMPORARIOS
        movq    %rdi,%r8    # t8 = n[]
        movl    %esi,%r9d   # t9 = b

        ; movq    $base,%rdi
        ; xorq    %rax,%rax     
        ; call    printf        # printf("base: %d", base);

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

        cmpl    %r9d,%r10d      # num < base    
        jb     	VALID           # in representation range
	movq    $-1, %rax       # ret = -1
        jmp     BigIntRead_return  
VALID:  
        ; movq    $fmt,%rdi
        ; movq    %r10,%rsi
        ; xorq    %rax,%rax
        ; call    printf          # printf("num: %hd", num);

        incl    %r15d
buffer_loop_test:
        cmpq    %r14,%r15         # i < size
        jb      buffer_loop_body

BigIntRead_return:
        # deallocate the local variable and restore the stack
        # to where it was at the beginning of the function
        addq	$4104,%rsp
        popq	%rbp
        ret
