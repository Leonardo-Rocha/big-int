                        .section       .rodata
msg:    .asciz "%c"
BigInt: .fill 512              # n[512] = {0}

        .text
        .globl _start, BigIntRead
 
_start: 
        movq $BigInt,%rdi # n = BigInt
        movq $10,%rsi     # b = 10
        call BigIntRead   # BigIntRead(BigInt, 10);

        movq $60,%rax    # exit syscall
        movq $0,%rdi     # return value: exit(0)
        syscall

// int BigIntRead(BigInt n, int b);
// %rdi = n; rsi = b;      
BigIntRead:
        pushq	%rbp
        subq	$4104,%rsp	# Allocate a buffer for 4097-char string
                    # Stack must be 16-byte aligned
                    # 4097+8 = 4105, but 4105 is not
                    # multiple of 16. 4104+8=4112, which
                    # is multiple of 16.

        movq    %rdi,%rcx   # x = n[]
        movl    %esi,%r9d;  # t9 = b
        movl    $0,%r15d    # i = 0

        movl	$0,%eax		# sys_read
        movl	$0,%edi		# Standard input
        leaq	(%rsp),%rsi	# Address of the local buffer - buf[4096]
        movl	$4097,%edx	# Maximum length of the input string
        syscall

        movl    %esi,%r10d   # r10d = buf[0]

        # deallocate the local variable and restore the stack
        # to where it was at the beginning of the function
        addq	$4104,%rsp
        popq	%rbp
        ret
