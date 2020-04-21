        .section       .data
        
fmt:    .asciz "%hd\n"
base:   .asciz "base: %d\n"
BigInt: .fill 512              # n[512] = {0}

        .text
        .globl _start, BigIntRead, BigIntPrint, BigIntToStr
        .globl CharToNumber, Log2, BigIntScale_by10, BigIntDiv10
        .extern printf
 
_start: 
        movq $BigInt,%rdi # n = BigInt
        movq $10,%rsi     # b = 10

        call BigIntRead   # BigIntRead(BigInt, 10);

        movq $60,%rax    # exit syscall
        movq $0,%rdi     # return value: exit(0)
        syscall

// int Log2(int num) returns the Log2 of a power of 2
// %rdi = num
Log2:
        xorq    %rax, %rax      # num_shifts = 0
        jmp     log2_test
log2_body:
        shrq    %rdi            # rdi >>= 1
        incl    %eax            # num_shifts++
log2_test:
        testq   $1,%rdi         # while(!(rdi & 1))
        je     log2_body
        ret                     # return num_shifts

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
// rdi = n[]; rsi = base;      
/*
 * rbx = n[] 
 * r8 = b (base - 2, 8, 10, 16)
 * r9 = log2(base)
 * rcx = i -> buffer interator
 * rdi = buffer_size
 * rdx = buf[i] (data) -> same register after CharToNumber conversion
 * eax = 32 bits accumulator 
 * r10 = k -> iterates int by int (4 bytes)
 * r11 = shifts_counter
 * r12 = reverse_counter
*/
BigIntRead:
        pushq	%rbp
        subq	$4104,%rsp	# Allocate a buffer for 4097-char string
                    # Stack must be 16-byte aligned
                    # 4097+8 = 4105, but 4105 is not
                    # multiple of 16. 4104+8=4112, which
                    # is multiple of 16.

        movq    %rdi,%rbx       # rbx = n[]
        movl    %esi,%r8d       # t8 = b

        movl    %r8d,%edi       # edi = base
        call    Log2            # rax = log2(base)
        
        movq    %rax,%r9        # t9 = log2(base)
        
        movl	$0,%eax		# sys_read
        movl	$0,%edi		# Standard input
        leaq	(%rsp),%rsi	# Address of the local buffer - buf[4096]
        movl	$4097,%edx	# Maximum length of the input string
        syscall                 # sys_read(stdin, buf, 4097)

        # Loop through the buffer and store each value read in n[].
        movl    $0,%ecx         # i = 0
        movl    %eax,%edi       # t14 = size
        decl    %edi            # size--
        # k_max = ((size - 1) * log2(base)) >> 3
        movl    %edi,%r12d      # reverse_counter = size
        decl    %r12d           # reverse_counter = (size - 1)
        movl    %r12d,%eax      # copy reverse_counter to multiplicand
        imull   %r9d            # (size - 1) * log2(base) 
        movl    %eax, %r12d     # store lower 4 bytes at r12d. 
        sarl    $3,%r12d        # divide by 8 to get the number of bytes
        jmp     buffer_loop_test
buffer_loop_body:    
        xorq    %rdx,%rdx
        movb    (%rsp,%rcx),%dl # num = buf[i]

        pushq   %rdi
        movq    %rdx,%rdi 
        call    CharToNumber    
        movl    %eax,%edx
        popq    %rdi

        xorl    %eax,%eax       # accumulator = 0
        xorl    %r10d,%r10d     # k = 0
        xorl    %r11d,%r11d     # shifts_counter = 0
        # After reading the input string, you must check if
	# it is valid. For example, a base-2 number should have
	# only the digits [0-1], a base-10 number should have
	# only the digits 0-9, and so on.
        cmpl    %r8d,%edx       # num < base    
        jb     	VALID           # in representation range
	movq    $-1,%rax        # ret = -1                      
        jmp     BigIntRead_return  
VALID:  
        cmpl    $10,%r8d 
        je      DECIMAL         # if(base == 10) 

        pushq   %rcx
        movl    %r9d,%ecx       # sal must be used with cl      
        sall    %cl,%eax        # accumulator <<= log2(base); to insert in the least significant bit    
        addl    %edx,%eax       # accumulate in the 32 bits
        popq    %rcx            

        testl   $8,%r11d                # if shifts_counter != 8
        jne     NO_MEM_STORE            # do not store de value in the BigInt, because it is not ready yet 
        movl    %eax,(%rbx,%r12)        # n[k] = value
        movl    $0,%r11d                # shifts_counter = 0 ; flush the counter
        subl    %r9d,%r12d              # reverse_counter -= log2(base) 
        jmp     NO_MEM_STORE
DECIMAL:    
        addl    %edx,%eax               # accumulate in the 32 bit
        pushq   %rdi                    # save buffer_size
        movq    %rbx,%rdi               # argument BigInt
        movl    %r10d,%esi              # argument numBytes
        call    BigIntScale_by10        # %rdi = n[] ,%rsi = numBytes
        popq    %rdi                    # restore buffer_size
        movl    %eax,(%rbx)             # n[] = value    
NO_MEM_STORE:    
        incl    %r11d                   # shifts_counter++ ; counts another shift
        addl    %r9d,%r10d              # k = k + log2(base)     
        xorl    %eax,%eax               # empties accumulator
        incl    %ecx                    # i++
buffer_loop_test:                       
        cmpq    %rdi,%rcx               # i < size 
        jb      buffer_loop_body        
BigIntRead_return: 
        # back to where it was at the beginning of the function
        addq	$4104,%rsp
        popq	%rbp
        ret

BigIntScale_by10:
        # rdi = BigInt n[], rsi = numBytes 
        movl    $0,%r14d                # i = 0;
        jmp     scale_cond              
scale_body:
        movw    (%rdi),%r12w            # get the lowest 16 bits from the BigInt
        andl    $0x0000FFFF,%r12d       # zero the higher 16 bits
        sall    %r12d                   # shift once to multiply n[k]*2
        movl    %r12d,%r13d             # copy n[k]*2
        sall    $2,%r12d                # shift once to get n[k]*8
        addl    %r13d,%r12d             # add to get n[k]*10
        movw    %r12w,(%rdi)            # attribute back to n[k]
        sarl    $16,%r12d               # cleans the attributed word
        incl    %r14d                   # i++
scale_cond:
        cmpl    %r14d,%esi              # i < numBytes
        jne     scale_body              
        ret 

// Print a BigInt n in the base b.		
// The base can be 2, 8, 10 or 16.		
// int BigIntPrint(BigInt n, int b);	
// rdi = n[]; rsi = base;
BigIntPrint:
        pushq	%rbp
        subq    $512,%rsp

        movq    %rsi,%rdx
        movq    %rsp,%rsi
        call    BigIntToStr

        xorl    %ecx,%ecx       # i = 0
print: 
        movq    $1,%rax
        movq    $1,%rdi
        xorq    %rsi,%rsi
        addq    %rsp,%rsi
        addq    %rcx,%rsi
        movq    $1,%rdx
        syscall                 # sys_write(stdout, rsp + i, 1)
print_condition:
        incl    %ecx            # i++
        movq    (%rsp,%rcx),%r8 # t8 = *(rsi + i)
        cmpl    $0,%r8
        jne     print

        # free the stack
        addq	$512,%rsp
        popq	%rbp
        ret 

// Convert n to string in the base b.		
// The base can be 2, 8, 10 or 16.		
// char *BigIntToStr(BigInt n, char *buf, int b); 
// rdi = n, rsi = *buf, rdx = b
// rbx = n[] 
// r8 = b (base - 2, 8, 10, 16)
// r9 = log2(base)
// r10 = i
// r11 = clear_shift
BigIntToStr:
        movq    %rdi,%rbx
        movq    %rdx,%r8

        movl    %r8,%rdi
        call    Log2    
        movl    %eax,%r9d               # r9 = log2(base)

        movl    $512,%r10               # i = max size
        jmp     consume_zeros_condition
consume_zeros_body:
        decl    %r10d                    # i--
consume_zeros_condition:
        movl    (%rbx,%r10),%edx        # edx = n[i]
        cmpl    %edx,$0                 # while(n[i] == 0)
        je      consume_zeros_body
        
        cmpl    $10,%r8d 
        je      set_decimal     # if(base == 10) 
        jmp     power_of_two_condition
power_of_two_body:
        movl    (%rbx,%r10),%edx        # edx = n[i]
        movl    $4,%r11d
        subq    %r9d,%r11d              # clear_shift = 4 - log2(base)
        movl    %r11d,%ecx               # sal must be used with cl  
        sall    %cl,%edx                # edx << clear_shift
        sarl    %cl,%edx                # edx >> clear_shift
        
        # loop interno dentro dos bytes seguindo a base
        # converter para char e mandar para (rsi + i) buff[i]
        # atribuir '/0' no final
        
        decl    %r10d                   # i--
power_of_two_condition:
        cmpl    %r10,$0                 # loopar até i (buf_size) >= 0
        jae     power_of_two_body
set_decimal:
        # TODO: fill code

        ret

# Bigint n = %rdi
BigIntDiv10: {
        pushq   %r10
        pushq   %r11
        pushq   %r12
        movl    (%rdi),%r10d       # n[]
        movl    %r10d,%r12d
        sarl    %r10d
        movl    %r10d,r11d
        sarl    %r11d
        addl    %r11d,r10d
        movl    %r10d,%r11d
        sarl    $4,%r11d
        addl    %r11d,%r10d        q = q + (q >> 4)
        movl    %r10d,%r11d
        sarl    $8,%r11d
        addl    %r11d,%r10d        q = q + (q >> 8)
        movl    %r10d,%r11d
        sarl    $16,%r11d
        addl    %r11d,%r10d
        sarl    $3,%r10d           q = q >> 3
        movl    %r10d,%r11d
        sall    $2,%r10d
        addl    %r11d,%r10d
        sall    %r10d
        subl    %r10d,%r12d        r = n - (((q << 2) + q) << 1)
        cmpl    $9,%r12d           (r > 9)
        jl      DIV_RETURN
        incl    %r11d 
DIV_RETURN
        %movl   %r11d,%eax
        ret
}
