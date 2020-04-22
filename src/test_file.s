        .section       .data
        
fmt:    .asciz "%hd\n"
base:   .asciz "base: %d\n"
BigInt: .fill 512              # n[512] = {0}

        .text
        .globl _start, BigIntRead, BigIntPrint, BigIntToStr
        .globl CharToNumber, Log2, BigIntScale_by10, BigIntDiv10
        .globl NumberToChar
        .extern printf
 
_start: 
        movq $BigInt,%rdi # n = BigInt
        movq $2,%rsi     # b = 10

        call BigIntRead   # BigIntRead(BigInt, 10);

        movl    $511,%r10d               # i = max size
        jmp     consume_zeros_condition_main
consume_zeros_body_main:
        decl    %r10d                    # i--
consume_zeros_condition_main:
        movq    $BigInt,%rdi
        movb    (%rdi,%r10),%dl        # edx = n[i]
        cmpb    $0,%dl                 # while(n[i] == 0)
        je      consume_zeros_body_main

        incl    %r10d
        jmp     print_main_cond
print_main_body:
        decl    %r10d
        pushq   %r10
        xorq    %rdx,%rdx
        movq    $BigInt,%rdi
        movb    (%rdi,%r10),%dl        # edx = n[i]
        movq    $fmt,%rdi
        movl    %edx,%esi
        xorq    %rax,%rax
        call    printf 
        popq    %r10
print_main_cond:
        cmpl    $0,%r10d
        ja      print_main_body

        movq $60,%rax     # exit syscall
        movq $0,%rdi      # return value: exit(0)
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
        je      log2_body
        ret                     # return num_shifts

// int CharToNumber(int c)
// %rdi = c 
CharToNumber:
        subq    $48,%rdi        # %rdi -= - 48 ; transforms char into number
        cmpq    $10,%rdi        # %rdi >= $10 ;  
        jb      number_return    
        subq    $7,%rdi         # Convertion for uppercase characters
        cmpq    $42,%rdi
        jb      number_return     
        subq    $32,%rdi        # Convertion for lowercase characters
number_return:
        movq    %rdi,%rax
        ret          

// char NumberToChar(int num)
// %rdi = num
NumberToChar: 
        addq    $48,%rdi        # %rdi += 48 ; transforms number into char
        cmpq    $58,%rdi        # %rdi >= 58
        jb      char_return     
        addq    $7,%rdi         # Convertion for uppercase characters
char_return:
        movq    %rdi,%rax
        ret

// int BigIntRead(BigInt n, int base);
// rdi = n[]; rsi = base;      
// Used registers:
// rbx = n[] 
// r8 = b (base - 2, 8, 10, 16)
// r9 = log2(base)
// rcx = i -> buffer interator
// rdi = buffer_size
// rdx = buf[i] (data) -> same register after CharToNumber conversion
// eax = 32 bits accumulator 
// r10 = k -> iterates int by int (4 bytes)
// r11 = shifts_counter
// r12 = reverse_counter
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
        # TODO: use a 2nd buffer to skip blank spaces

        
        movl    $0,%ecx         # i = 0
        movl    %eax,%edi       # t14 = size
        decl    %edi            # size-- to remove '\0'
        # k_max = (size * log2(base)) >> 3
        movl    %edi,%r12d      # reverse_counter = size
BREAK:        
        movl    %r12d,%r13d     # t13 = reverse_counter
        sarl    $3,%r12d        # divide by 8 to get the number of entries
        andl    $0x07,%r13d     # t13 %= 8
        cmpl    $0,%r13d        
        je      no_RC_increment
        incl    %r12d           # sum to allign by Bytes
no_RC_increment:
        movl    %r12d,%eax      # copy reverse_counter to multiplicand
        imull   %r9d            # numEntry * log2(base) 
        movl    %eax,%r12d      # store lower 4 bytes at r12d.      

        movl    %edi,%r13d      # t13 = size 
        andl    $0x07,%r13d     # t13 = size % 8
        movl    $8,%r11d        # shifts_counter = 8 
        subl    %r13d,%r11d     # shifts_counters = (8 - size%8) -> this is used to fix incomplete bytes sequences
        andl    $0x07,%r11d     # shifts_counters =% 8
        incl    %r11d           # shifts_counters++
        xorl    %r10d,%r10d     # k = 0
        xorl    %eax,%eax       # accumulator = 0
        # Loop through the buffer and store each value read in n[].
        jmp     buffer_loop_test
buffer_loop_body:    
        xorq    %rdx,%rdx
        movb    (%rsp,%rcx),%dl # num = buf[i]
        
        pushq   %rdi
        pushq   %rax
        movq    %rdx,%rdi 
        call    CharToNumber    
        movl    %eax,%edx
        popq    %rax
        popq    %rdi

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

        cmpl    $8,%r11d                # if shifts_counter < 8
        jb      increment_counters      # do not store the value in the BigInt, because it is not ready yet 
        subl    %r9d,%r12d              # reverse_counter -= log2(base)
        movl    %eax,(%rbx,%r12)        # n[reverse_counter] = value
        movl    $0,%r11d                # shifts_counter = 0 ; flush the counter
        jmp     increment_counters
DECIMAL:    
        pushq   %rdi                    # save buffer_size
        movq    %rbx,%rdi               # argument BigInt
        movl    %r10d,%esi              # argument numBytes
        call    BigIntScale_by10        # %rdi = n[], %rsi = numBytes 
        popq    %rdi                    # restore buffer_size
        movl    (%rbx),%eax             # accumulator = n[0]
        addl    %edx,%eax               # accumulator += value
        movl    %eax,(%rbx)             # n[0] = value    
        incl    %r10d                   # k++
increment_counters:    
        incl    %r11d                   # shifts_counter++ ; counts another shift     
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
        xorq    %r15,%r15
        movl    $0,%r14d                # i = 0;
        sarl    %esi                    # numBytes = numBytes /2
        incl    %esi                    # guarantee at least one execution
        cmpl    $256,%esi               # numBytes < 256
        jb      scale_cond              # verify if we are trying to multiply bytes that do not exist
        movl    $256,%esi               
        jmp     scale_cond              
scale_body:
        movw    (%rdi,%r14,2),%r12w     # get the lowest 16 bits from the BigInt
        andl    $0x0000FFFF,%r12d       # zero the higher 16 bits
        sall    %r12d                   # shift once to multiply n[k]*2
        movl    %r12d,%r13d             # copy n[k]*2
        sall    $2,%r12d                # shift once to get n[k]*8
        addl    %r13d,%r12d             # add to get n[k]*10
        addl    %r15d,%r12d             # sum with previous multiply result
        movw    %r12w,(%rdi,%r14,2)     # attribute back to n[k]
        movl    %r12d,%r15d             # stores the last multiply result
        sarl    $16,%r15d               # cleans the attributed word
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
        cmpl    $0,%r8d
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
        movq    %r8,%rdi
        call    Log2    
        movl    %eax,%r9d               # r9 = log2(base)

        movl    $511,%r10d               # i = max size
        jmp     consume_zeros_condition
consume_zeros_body:
        decl    %r10d                    # i--
consume_zeros_condition:
        movb    (%rbx,%r10),%dl        # edx = n[i]
        cmpb    $0,%dl                 # while(n[i] == 0)
        je      consume_zeros_body
        
        cmpl    $10,%r8d 
        je      set_decimal     # if(base == 10) 
        jmp     power_of_two_condition
power_of_two_body:
        movl    (%rbx,%r10),%edx        # edx = n[i]
        movl    $4,%r11d
        subl    %r9d,%r11d              # clear_shift = 4 - log2(base)
        movl    %r11d,%ecx              # sal must be used with cl  
        sall    %cl,%edx                # edx << clear_shift
        sarl    %cl,%edx                # edx >> clear_shift
        
        # loop interno dentro dos bytes seguindo a base
        # converter para char e mandar para (rsi + i) buff[i]
        
        decl    %r10d                   # i--
power_of_two_condition:
        cmpl    $0,%r10d                 # loopar até i (buf_size) >= 0
        jae     power_of_two_body
set_decimal:
        # TODO: fill code
        
        movb    $0,(%rsi,%r10)          # add '\0' in the end of the string  
        ret

# Bigint n = %rdi, numBytes = %rsi
BigIntDiv10: 
        pushq   %rbp            # store frame
        pushq   %r10            # store temporary
        pushq   %r11            # store temporary
        pushq   %r12            # store temporary
        pushq   %r13            # store temporary
DIV_BODY:        
        decl    %esi                    # size--
        xorq    %r10,%r10               # make sure q=0
        movl    (%rsi,%rdi),%r10d       # q = n[size]
        movl    %r10d,%r12d             # n = q
        sarl    %r10d                   # q >> 1
        movl    %r10d,%r11d              # tq = q
        sarl    %r11d                   # tq >> 1
        addl    %r11d,%r10d              # q = q + tq; q = (n>>1)+ (n>>2)
        movl    %r10d,%r11d             # tq = q
        sarl    $4,%r11d                # tq>>4
        addl    %r11d,%r10d             # q = q + (q >> 4)
        movl    %r10d,%r11d             # tq = q
        sarl    $8,%r11d                # tq>>8
        addl    %r11d,%r10d             # q = q + (q >> 8)
        movl    %r10d,%r11d             # tq = q
        sarl    $16,%r11d               # tq>>16
        addl    %r11d,%r10d             # q = q + q>>16
        sarl    $3,%r10d                # q = q >> 3
        movl    %r10d,%r11d             # tq = q
        sall    $2,%r10d                # q<<2
        addl    %r11d,%r10d             # q = (q<<2) + q 
        sall    %r10d                   # q<<1
        subl    %r10d,%r12d             # n = n - (((q << 2) + q) << 1)
        cmpl    $9,%r12d                # (n > 9)
        jl      DIV_COND
        incl    %r11d                   # q++
        subl    $10,%r12d               # n = n -10
DIV_COND:
        addl    %r13d,%r11d             # q = q + r
        movl    %r11d,(%rsi,%rdi)       # n[size] = q
        movl    %r12d,%r13d             # r = n
        cmpl    $0,%esi                 # size == 0
        jg      DIV_BODY
DIV_RETURN:
        movl    %r13d,%eax      # return r ; r = Bigint % 10
        popq    %r10            # restore temporary
        popq    %r11            # restore temporary
        popq    %r12            # restore temporary
        popq    %r13            # restore temporary
        popq    %rbp            # restore frame
        ret
