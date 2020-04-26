                .section       .data

two_p_32String: .asciz "100000000"     # 2^32 
fmt:            .asciz "%hd\n"
base:           .asciz "base: %d\n"
BigInt:         .fill 512               # n[512] = {0}

        .text
        .globl _start, BigIntRead, BigIntPrint, BigIntToStr
        .globl CharToNumber, Log2, BigIntScale_by10, BigIntDiv10
        .globl NumberToChar, CalculateReadReverseCounter, _BigIntRead
        .globl CalculateWriteReverseCounter
        .globl BigIntAdd, BigIntNeg, BigIntSub, BigIntShl, BigIntShar
        .globl BigIntMul, BigIntAssign, BigIntMod, BigIntDiv
        .globl IsBigIntNeg, GetBigIntSizeInBits
        .globl BigIntEq, BigIntGT, BigIntLT, BigIntXor, BigIntOr, BigIntAnd
        .extern printf
 
_start: 
        movq    $BigInt,%rdi # n = BigInt
        movq    $2,%rsi     # b = 2
        call    BigIntRead   # BigIntRead(BigInt, 2);
        
        movq    $BigInt,%rdi # n = BigInt
        movq    $2,%rsi     # b = 2
        call    BigIntPrint  # BigIntPrint(BigInt, 2)

        movq    $60,%rax     # exit syscall
        movq    $0,%rdi      # return value: exit(0)
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
// r8  = rdi 
// r9 = rsi      
BigIntRead:
        pushq	%rbp
        pushq   %rcx
        pushq   %r8
        pushq   %r9
        subq	$4104,%rsp	# Allocate a buffer for 4097-char string
                    # Stack must be 16-byte aligned
                    # 4097+8 = 4105, but 4105 is not
                    # multiple of 16. 4104+8=4112, which
                    # is multiple of 16.
        movq    %rdi,%r8 
        movq    %rsi,%r9 

        movl	$0,%eax		# sys_read
        movl	$0,%edi		# Standard input
        leaq	(%rsp),%rsi	# Address of the local buffer - buf[4096]
        movl	$4097,%edx	# Maximum length of the input string
        syscall                 # sys_read(stdin, buf, 4097)

        movq    %r8,%rdi 
        movq    %r9,%rsi
        movq    %rsp,%rdx
        movl    %eax,%ecx
        call    _BigIntRead     # _BigIntRead(n, base, buf, size)

        # back to where it was at the beginning of the function
        addq	$4104,%rsp
        popq    %r9 
        popq    %r8
        popq    %rcx
        popq	%rbp
        ret

// int _BigIntRead(BigInt n, int base, char *buffer, size)
// rdi = n[]; rsi = base; rdx = buffer; rcx = size
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
// r13 = buffer
// r14 = is_negative
// r15 = two_p_32BigInt
_BigIntRead:
        pushq   %rbx
        pushq   %r8
        pushq   %r9
        pushq   %r10
        pushq   %r11 
        pushq   %r12 
        pushq   %r13
        pushq   %r14
        pushq   %r15
        pushq   %rdx

        movq    %rdi,%rbx       # rbx = n[]
        movl    %ecx,%edi       # size = ecx
        movl    $0,%ecx         # i = 0  
        movl    %esi,%r8d       # t8 = b    
        decl    %edi            # size-- to remove '\0'

        pushq   %rdi
        movl    %r8d,%edi       # edi = base
        call    Log2            # rax = log2(base)
        movq    %rax,%r9        # t9 = log2(base)
        popq    %rdi
        # Decimal overflow handling
        cmpl    $10,%r8d 
        jne     no_decimal_handling
        // Allocate a temporary BigInt
        subq    $512,%rsp
        movq    (%rsp),%r15
        // Store caller-saved registers
        pushq   %rdi
        pushq   %rsi
        pushq   %rdx 
        pushq   %rcx
        // Create a BigInt from constant
        movq    %r15,%rdi               # argument BigInt n    
        movl    $16,%esi                # argument base
        movq    $two_p_32String,%rdx    # argument buffer
        movl    $9,%ecx                 # argument size
        call    _BigIntRead
        popq   %rdi
        popq   %rsi
        popq   %rdx 
        popq   %rcx    
no_decimal_handling:
        pushq   %rdi
        movl    %r9d,%esi       # rsi = log2(base)
        call    CalculateReadReverseCounter   
        movl    %eax,%r12d      # store lower 4 bytes at r12d.
        popq    %rdi
        movl    %edi,%r13d      # t13 = size 
        andl    $0x07,%r13d     # t13 = size % 8
        movl    $8,%r11d        # shifts_counter = 8 
        subl    %r13d,%r11d     # shifts_counters = (8 - size%8) -> this is used to fix incomplete bytes sequences
        andl    $0x07,%r11d     # shifts_counters =% 8
        incl    %r11d           # shifts_counters++
        xorl    %r10d,%r10d     # k = 0
        xorl    %eax,%eax       # accumulator = 0
        # Loop through the buffer and store each value read in n[].    
        popq    %rdx            
        movq    %rdx,%r13      # t13 = buffer
        # signal checking
        movl    $0,%r14d        # is_negative = 0
        xorq    %rdx,%rdx
        movb    (%r13,%rcx),%dl # num = buf[0]
        cmpl    $43,%edx        # buf[0] == '+'
        jne     not_plus
        incl    %ecx
not_plus:
        cmpl    $45,%edx        # buf[0] == '-'
        jne     buffer_loop_test
        incl    %ecx
        movl    $1,%r14d        # is_negative = 1        
        jmp     buffer_loop_test
buffer_loop_body:    
        xorq    %rdx,%rdx
        movb    (%r13,%rcx),%dl # num = buf[i]
        
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
        jno     no_overflow           
        
        // Store caller-saved registers
        pushq   %rdi
        pushq   %rsi
        pushq   %rdx 
        // sum a 2^32 BigInt to solve overflow
        movq    %r15,%rdi               # n = two_pow_32[]
        movq    %rbx,%rsi               # y = n[]         
        movq    %rbx,%rdx               # xpy = n[]
        call    BigIntAdd               # BigIntAdd(two_p_32BigInt, n, n)
        // Restore caller-saved registers
        popq    %rdx 
        popq    %rsi 
        popq    %rdi 
no_overflow:
        movl    %eax,(%rbx)             # n[0] = value    
        incl    %r10d                   # k++
increment_counters:    
        incl    %r11d                   # shifts_counter++ ; counts another shift     
        incl    %ecx                    # i++
buffer_loop_test:                       
        cmpq    %rdi,%rcx               # i < size 
        jb      buffer_loop_body        
        
        xorq    %rax,%rax               # ret = 0
        # signal correction
        cmpl    $1,%r14d
        jne     BigIntRead_return
        movq    %rbx,%rdi               # argument BigInt x
        call    BigIntNeg
BigIntRead_return: 
        // deallocate overflow BigInt 
        cmpl    $10,%r8d 
        jne     no_dealloc
        addq    $512,%rsp
no_dealloc:        
        popq    %r15
        popq    %r14
        popq    %r13
        popq    %r12
        popq    %r11
        popq    %r10 
        popq    %r9
        popq    %r8 
        popq    %rbx
        ret

// void BigIntScale_by10(BigInt n, int numBytes)
// rdi = BigInt n[], rsi = numBytes 
BigIntScale_by10:
        pushq   %r12
        pushq   %r13
        pushq   %r14
        pushq   %r15
        xorq    %r15,%r15
        movl    $0,%r14d                # i = 0;
        shrl    %esi                    # numBytes = numBytes /2
        incl    %esi                    # guarantee at least one execution
        cmpl    $256,%esi               # numBytes < 256
        jb      scale_cond              # verify if we are trying to multiply bytes that do not exist
        movl    $256,%esi               
        jmp     scale_cond              
scale_body:
        movw    (%rdi,%r14,2),%r12w     # get the lowest 16 bits from the BigInt
        andl    $0x0000FFFF,%r12d       # zero the higher 16 bits
        shll    %r12d                   # shift once to multiply n[k]*2
        movl    %r12d,%r13d             # copy n[k]*2
        shll    $2,%r12d                # shift once to get n[k]*8
        addl    %r13d,%r12d             # add to get n[k]*10       
        addl    %r15d,%r12d             # sum with previous multiply result
        movw    %r12w,(%rdi,%r14,2)     # attribute back to n[k]
        movl    %r12d,%r15d             # stores the last multiply result
        shrl    $16,%r15d               # cleans the attributed word
        incl    %r14d                   # i++
scale_cond:
        cmpl    %r14d,%esi              # i < numBytes
        jne     scale_body 
        popq    %r15
        popq    %r14
        popq    %r13
        popq    %r12                      
        ret 

// Print a BigInt n in the base b.		
// The base can be 2, 8, 10 or 16.		
// int BigIntPrint(BigInt n, int b);	
// rdi = n[]; rsi = base;
BigIntPrint:
        pushq	%rbp
        pushq   %rbx 
        pushq   %rcx
        pushq   %rdx   
        pushq   %r8
        pushq   %r9 
        subq    $4098,%rsp

        movq    %rsi,%rdx
        movq    %rsp,%rsi
        call    BigIntToStr
        movq    %rax,%rbx        
        
        # do not print zeros on the left.
        xorl    %ecx,%ecx               # i = 0
        jmp     consume_zeros_print_condition
consume_zeros_print_body:
        incl    %ecx                    # i++
consume_zeros_print_condition:
        movb    (%rbx,%rcx),%dl         # edx = n[i]
        cmpb    $48,%dl                 # while(n[i] == '0')
        je      consume_zeros_print_body      
        
        xorl    %r9d,%r9d
print:  
        pushq   %rcx
        movq    $1,%rax
        movq    $1,%rdi
        movq    %rbx,%rsi
        addq    %rcx,%rsi
        movq    $1,%rdx
        syscall                         # sys_write(stdout, rsp + i, 1)
        cmpl    $-1,%eax
        je      print_return
        incl    %r9d                    # t9++
        popq    %rcx
print_condition:
        incl    %ecx                    # i++
        movb    (%rbx,%rcx),%r8b        # t8 = *(rsi + i)
        cmpb    $0,%r8b
        jne     print
        movq    $10,(%rbx,%rcx)         # print '\n'
        
        movq    $1,%rax
        movq    $1,%rdi
        movq    %rbx,%rsi
        addq    %rcx,%rsi
        movq    $1,%rdx
        syscall                         # sys_write(stdout, rsp + i, 1)
        
        movl    %r9d,%eax               # ret = t9
print_return:
        # free the stack
        addq	$4098,%rsp
        popq    %r9
        popq    %r8 
        popq    %rdx 
        popq    %rcx 
        popq    %rbx 
        popq    %rbp 
        ret 

// Convert n to string in the base b.		
// The base can be 2, 8, 10 or 16.		
// char *BigIntToStr(BigInt n, char *buf, int b); 
// rdi = n, rsi = *buf, rdx = b
// rbx = n[] 
// r8 = b (base - 2, 8, 10, 16)
// r9 = log2(base) // decimal_copy
// r10 = i
// r11 = reverse_counter
BigIntToStr:
        pushq   %rcx 
        pushq   %rbx 
        pushq   %r8 
        pushq   %r9 
        pushq   %r10 
        pushq   %r11
        pushq   %r12 
        pushq   %r13 
        pushq   %r14 

        movq    %rdi,%rbx
        movq    %rdx,%r8
        movq    %r8,%rdi
        call    Log2    
        movl    %eax,%r9d               # r9 = log2(base)

        movl    $511,%r10d              # i = max size
        jmp     consume_zeros_condition
consume_zeros_body:
        decl    %r10d                   # i--
consume_zeros_condition:
        cmpl    $0,%r10d        
        jl      to_string_return        # end_consume if r10 < 0
        movb    (%rbx,%r10),%dl 
        cmpb    $0,%dl                  # while(n[i] == 0)
        je      consume_zeros_body

        cmpl    $10,%r8d 
        je      decimal_case            # if(base == 10) 

        pushq   %rsi                    # store buffer
        movl    %r10d,%edi              # rdi = size
        movl    %r9d,%esi               # rsi = log2(base)
        incl    %edi                    # size++
        call    CalculateWriteReverseCounter # CalculateReadReverseCounter(size, log2Base)
        movl    %eax,%r11d              # reverse_counter = ret
        popq    %rsi                    # restore buffer

        xorl    %r13d,%r13d             # j = 0
        jmp     power_of_two_condition
power_of_two_body:
        movl    (%rbx,%r11),%edx        # edx = n[reverse_counter]
        subl    %r9d,%r11d              # reverse_counter -= log2(base)
        movl    $32,%r12d               # shift_width 
        movl    %r9d,%r14d              # t14 = log2(base)
        shll    $3,%r14d                # t14 *= 8
        subl    %r14d,%r12d             # t12 = 32 - log2(base) * 8
        movl    %r12d,%ecx              # sal must be used with cl
        shll    %cl,%edx
        movl    $32,%r12d               # shift_width 
        subl    %r9d,%r12d              # shift_width = 32 - log2(base)
        xorl    %r14d,%r14d             # k = 0
        jmp     byte_cond
byte_body:
        movl    %edx,%eax               # store edx in a temporary
        movl    %r12d,%ecx              # sar must be used with cl      
        shrl    %cl,%eax                # accumulator >>= log2(base); to move the desired write to the least significat byte    
        
        movl    %r9d,%ecx               # sal must be used with cl
        shll    %cl,%edx                # erase already written bits

        movl    %eax,%edi               # argument num
        call    NumberToChar            # NumberToChar(num)
        movb    %al,(%rsi,%r13)         # buf[j] = char
        incl    %r13d                   # j++
        incl    %r14d                   # k++
byte_cond:
        cmpl    $8,%r14d                # if k < 8
        jb      byte_body              
power_of_two_condition:
        cmpl    $0,%r11d                # buf_size >= 0
        jge     power_of_two_body
        jmp     to_string_return

decimal_case:
        incl    %r10d

        subq    $512,%rsp               # allocate 512 Bytes in Stack for a BigIntCopy
        movq    (%rsp),%r9              # get the address of the local BigIntCopy
        
        pushq   %rdi
        pushq   %rsi
        movq    %r9,%rdi                # argument destination                
        movq    %rbx,%rsi               # argument source
        call    BigIntAssign
        movq    %r9,%rbx                # now rbx->copy
        popq    %rsi
        popq    %rdi
        
        # NOTE: calculation of correct max decimal chars might be unecessary, considering zeros are already trimmed in print 
        movl    %r10d,%r11d
        shll    $2,%r11d                # max decimal chars (insn't exact, only a superior limit)
        cmpl    $1237,%r11d             # max_decimal_chars > 1236
        jbe     in_string_limit
        movl    $1237,%r11d               
in_string_limit:
        movl    %r11d,%r13d             # string_end = size
        jmp     decimal_condition
decimal_body:
        decl    %r11d                   # m--
        pushq   %rsi
        movq    %rbx,%rdi               # argument BigInt x
        movl    %r10d,%esi              # argument numBytes (*)
        call    BigIntDiv10             # returns n % 10 in eax, and leave the BigInt divided by 10;
        popq    %rsi
        movl    %eax,%edi               # argument num
        call    NumberToChar            # NumberToChar(num)
        movb    %al,(%rsi,%r11)         # buf[m] = char
decimal_condition:
        cmpl    $0,%r11d        
        jge     decimal_body            # end if r11 < 0

to_string_return:        
        addq    $512,%rsp               # deallocate the BigIntCopy       
        
        movb    $0,(%rsi,%r13)          # add '\0' in the end of the string
        movq    %rsi,%rax               # ret = buffer
        popq    %r14 
        popq    %r13 
        popq    %r12 
        popq    %r11 
        popq    %r10 
        popq    %r9 
        popq    %r8 
        popq    %rbx 
        popq    %rcx 
        ret
// * Note that this does not decrement. the reason why we stick with the superior limit is because 
//   it would be costly, or even impossible, to verify when one byte 
//   no longer has information about the next printed decimal character.


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

// Size must be a number of characters
// int CalculateReadReverseCounter(int size, int log2Base)
// %rdi = size, %rsi = log2Base
CalculateReadReverseCounter:       
        pushq   %r13           
        # k_max = ceiling(size(chars) >> 3) * log2(base)
        movl    %edi,%r13d      # t13 = reverse_counter
        sarl    $3,%edi         # divide by 8 to get the number of entries
        andl    $0x07,%r13d     # t13 %= 8
        cmpl    $0,%r13d        
        je      no_RC_increment
        incl    %edi            # sum to allign by Bytes
no_RC_increment:
        movl    %edi,%eax       # copy reverse_counter to multiplicand
        imull   %esi            # numEntry * log2(base)    
        popq    %r13
        ret

// Size must be a number of bits
// int CalculateWriteReverseCounter(int size, int log2Base)
// %rdi = size, %rsi = log2Base
CalculateWriteReverseCounter:   # 1223334444 f(4,3) -> 3
        pushq   %r13            # 00F0F0F0     f(4,4) -> 0       
        pushq   %rcx            
        movl    %edi,%r13d      # t13 = reverse_counter = size
        cmpl    $3,%esi
        je      OCTAL_CASE
        movl    %esi,%ecx       # ecx = log2(base)
        decl    %ecx            # log2(base)--
        andl    %ecx,%r13d      # t13 %= log2(base)
        cmpl    $0,%r13d
        je      write_reverse_return
        movl    %esi,%ecx       # ecx = log2(base)
        subl    %r13d,%ecx      # ecx = log2(base) - size%log2(b)
        addl    %ecx,%edi       # sum to allign by entries
        jmp     write_reverse_return
OCTAL_CASE:    
        movl    %edi,%ecx               # ecx = size
        movl	%edi,%eax               # eax = size
        movl	$1431655766,%edx        # edx = magic number
        imull	%edx                    # edx = edx * size
        movl	%ecx,%eax               # eax = size
        sarl	$31,%eax                # eax = size>>31
        subl	%eax,%edx               # edx = magic number* size - size>>31
        movl	%edx,%eax               # eax = size/3 = div_result
        addl	%eax,%eax               # eax = div_result *2
	addl	%edx,%eax               # eax = div_result*3
	subl	%eax,%ecx               # ecx = size - div_result*3 = size%3
        cmpl    $0,%ecx                 # verifies if size%3 == 0
        je      write_reverse_return
        movl    %esi,%r13d              # t13 = log2(b)
        subl    %ecx,%r13d              # esi = log2(b)-size%3
        addl    %r13d,%edi              # sum to allign by entries
write_reverse_return:        
        subl    %esi,%edi               # size = size - log2(base)
        movl    %edi,%eax               # ret = reverse_counter
        popq    %rcx
        popq    %r13 
        ret

// BigIntSub: xmy = x - y	
// void BigIntSub(BigInt x, BigInt y, BigInt xmy);
// rdi = x; rsi = y; rdx = xmy	
// r8 = tempx
BigIntSub:
	pushq   %r8             # store temporary
        movq    %rdi,%r8        # t8 = x
        movq    %rsi,%rdx       # x = y
        call    BigIntNeg       # y = -y
        movq    %r8,%rdi        # x = t8
        call    BigIntAdd       # xmy = x - y
        popq    %r8             # restore temporary
        ret

// BigIntAdd: xpy = x + y				
// void BigIntAdd(BigInt x, BigInt y, BigInt xpy);	
// rdi = x; rsi = y; rdx = xpy
// rcx = i
// r8 = x[i]
// r9 = y[i]
// r10 = xpy[i]
// r11 = carry
BigIntAdd:
        pushq   %rcx
        pushq   %r8
        pushq   %r9
        pushq   %r10
        pushq   %r11
        movl    $0,%ecx         # i = 0
        movl    $0,%r8d         # x[i]
        movl    $0,%r9d         # y[i]
        movl    $0,%r10d        # xpy[i]
        movl    $0,%r11d        # carry
add_body:
        movl    (%rdi,%rcx,4),%r8d        # update x=x[i]
        movl    (%rsi,%rcx,4),%r9d        # update y=y[i]
        addl    %r8d,%r9d               # y = x+y
        setb    %r11b                   # carry flag
        movl    %r9d,%r10d              # xpy = x+y
        addl    %r11d,%r10d             # xpy += carry
        movl    %r10d,(%rdi,%rcx,4)       # xpy[i] = xpy
        incl    %ecx                    # i++
add_cond:
        cmpl    $128,%ecx               # i < 128       we operate 32 bits at a time
        jb      add_body                
        popq    %r11
        popq    %r10
        popq    %r9
        popq    %r8
        popq    %rcx
        ret

// BigIntNeg: x = ~x		
// void BigIntNeg(BigInt x);
BigIntNeg:
	pushq   %rcx
        pushq   %r8
        pushq   %r9
        movl    $0,%ecx         # i = 0
        movl    $0,%r8d         # x[i]
        movl    $1,%r9d         # carry
neg_body:
        movl    (%rdi,%rcx,4),%r8d        # update x=x[i]
        notl    %r8d
        addl    %r9d,%r8d              # x = -x+ carry
        setb    %r9b                   # carry flag
        movl    %r8d,(%rdi,%rcx,4)       # x[i] = -x
        incl    %ecx                   # i++
neg_cond:
        cmpl    $128,%ecx               # i < 128       we operate 32 bits at a time
        jb      neg_body                
        popq    %r9
        popq    %r8
        popq    %rcx
        ret

// BigIntMul: xty = x * y	
// void BigIntMul(BigInt x, BigInt y, BigInt xty);
// ARGUMENTS
// rdi = x
// rsi = y
// rdx = xty    
// LOCALS
// rcx = i
// r8  = num_shifts
// r9  = y_buffer
// r10 = address_aux / buffer_aux
// r11 = internal_counter : j
BigIntMul:
	pushq   %rcx
        pushq   %r8
        pushq   %r9
        pushq   %r10
        pushq   %r11
        movl    $0,%ecx         # i = 0
        movl    $1,%r8d         # num_shifts = 1
        pushq   %rsi
        movq    %rdi,%r10       # aux = x (address)
        movq    %rdx,%rdi       # x = xmy (address)
        movq    %r10,%rsi       # y = aux (address)
        call    BigIntAssign    # xmy = x (fully)
        popq    %rsi            # restore y address
        # from this moment on rdi -> xmy
        jmp     mul_condition
mul_body:
        movl    (%rsi,%rcx,4),%r9d        # y_buffer = y[i]
        movl    $0,%r11d                # j = 0
        jmp     mul_intern_condition
mul_intern_body:
        # TODO: if this is problematic, attempt cmpl and aux solution
        testl   $0x01,%r9d               # verifies if last bit is positive
        je      no_shift
        pushq   %rsi                    # stores y address
        movl    %r8d,%esi               # argument num_shifts
        call    BigIntShl               # xmy << num_shifts if last bit is positive
        popq    %rsi                    # restores y address
no_shift:
        incl    %r8d                    # get the next num_shifts value
        shrl    %r9d                    # y_buffer >> ; so we can check the next bit
mul_intern_condition:
        cmpl    $32,%r11d               # j < 32
        jb      mul_intern_body
        incl    %ecx                    # i++
mul_condition:
        cmpl    $128,%ecx               # i < 128
        jb      mul_body
        popq    %r11
        popq    %r10
        popq    %r9
        popq    %r8
        popq    %rcx
        ret     

// BigIntAssign: x = y				
// void BigIntAssign(BigInt x, BigInt y);
// rdi = x
// rsi = y
// rcx = i
// r8  = buffer	
BigIntAssign:
	pushq   %rcx
        pushq   %r8

        movl    $0,%ecx         # i = 0 
        jmp     assign_condition
assign_body:
        movl    (%rsi,%rcx,4),%r8d      # buffer = y[i]
        movl    %r8d,(%rdi,%rcx,4)      # x[i] = buffer
assign_condition:
        cmpl    $128,%ecx               # i < 128 we operate in 32 bits
        jb      assign_body

        popq    %r8
        popq    %rcx
        ret


// BigIntShl:   bi = bi << num_shifts
// rdi: BigInt bi
// rsi: int num_shifts
// rcx: current_shifts
// r8 : bi_buffer
// r9 : internal iterator : i
// r10: prev_shift_letfover 
BigIntShl:
        pushq   %rcx
        pushq   %r8
        pushq   %r9
        pushq   %r10                
        jmp     shl_condition
shl_body:
        movl    %esi,%ecx               # current_shifts = num_shifts
        andl    $0x01F,%ecx             # current_shifts % 32
        subl    %ecx,%esi               # calculate remaining shifts
        movl    $0,%r9d                 # i = 0
        movl    $0,%r10d                # prev_shift_leftover = 0
        jmp     shl_intern_condition
shl_intern_body:
        movl    (%rdi,%r9,4),%r8d       # bi_buffer = bi[i] ;32 bits at time 
        shlq    %cl,%r8                 # bi_buffer << current_shifts
        addl    %r10d,%r8d              # bi_buffer += prev_shift_leftover
        movl    %r8d,(%rdi,%r9,4)         # bi[i] = bi[i] << current_shifts
        # TODO: verify if it is possible to shift 32 at once
        shrq    $16,%r8
        shrq    $16,%r8                # cleans the attributed 32 bits
        movl    %r8d,%r10d              # store the leftover
shl_intern_condition:
        cmpl    $128 ,%r9d              # i < 128
        jb      shl_intern_body
shl_condition:
        cmpl    $0,%esi                 # num_shifts > 0
        ja      shl_body

        popq    %r10
        popq    %r9
        popq    %r8
        popq    %rcx
        ret

// BigIntShar:   bi = bi << num_shifts
// rdi: BigInt bi
// rsi: int num_shifts
// rcx: current_shifts
// r8 : bi_buffer
// r9 : internal iterator : i
// r10: prev_shift_letfover 
// r11: prev_shift_aux
BigIntShar:
        pushq   %rcx
        pushq   %r8
        pushq   %r9
        pushq   %r10
        pushq   %r11                
        jmp     shar_condition
shar_body:
        movl    %esi,%ecx               # current_shifts = num_shifts
        andl    $0x01F,%ecx             # current_shifts % 32
        subl    %ecx,%esi               # calculate remaining shifts
        movl    $128,%r9d               # i = 128
        movl    $0,%r10d                # prev_shift_leftover = 0
        jmp     shar_intern_condition
shar_intern_body:
        movl    -4(%rdi,%r9,4),%r8d     # bi_buffer = bi[i-1] ;32 bits at time 
        shlq    $31,%r8                 # bi_buffer << 31 ; so we dont lose info
        shrq    %cl,%r8                 # bi_buffer >> current_shifts
        movl    %r8d,%r11d              # store the leftover in aux
        shrq    $31,%r8                 # bi_buffer >> 31
        addl    %r10d,%r8d              # bi_buffer += prev_shift_leftover
        movl    %r8d,-4(%rdi,%r9,4)     # bi[i] = bi[i-1] >> current_shifts
        # TODO: verify if it is possible to shift 32 at once
        movl    %r11d,%r10d             # get the next leftover
        shll    %r10d                   # fix to complete 1 + 31 = 32
shar_intern_condition:
        cmpl    %r9d,0                  # i > 0
        ja      shar_intern_body
shar_condition:
        cmpl    $0,%esi                 # num_shifts > 0
        ja      shar_body
        popq    %r11
        popq    %r10
        popq    %r9
        popq    %r8
        popq    %rcx
        ret        


// BigIntAnd: xay = x & y	
// void BigIntAnd(BigInt x, BigInt y, BigInt xay);
// ARGUMENTS
// rdi = x
// rsi = y_buffer
// rdx = xay
// LOCALS
// rcx = i
// r8  = x_buffer
// r9  = y_buffer
BigIntAnd:
	pushq   %rcx
        pushq   %r8
        pushq   %r9

        movl    $0,%ecx         # i = 0 
        jmp     and_condition
and_body:
        movl    (%rdi,%rcx,4),%r8d      # x_buffer = x[i]
        movl    (%rsi,%rcx,4),%r9d      # y_buffer = y[i]
        andl    %r9d,%r8d               # x_buffer = x_buffer & y_buffer
        movl    %r8d,(%rdx,%rcx,4)      # xay[i] = buffer
and_condition:
        cmpl    $128,%ecx               # i < 128 we operate in 32 bits
        jb      and_body

        popq    %r9
        popq    %r8
        popq    %rcx
        ret
	
// BigIntOr: xoy = x | y	
// void BigIntOr(BigInt x, BigInt y, BigInt xoy);
// ARGUMENTS
// rdi = x
// rsi = y_buffer
// rdx = xay
// LOCALS
// rcx = i
// r8  = x_buffer
// r9  = y_buffer
BigIntOr:
	pushq   %rcx
        pushq   %r8
        pushq   %r9

        movl    $0,%ecx         # i = 0 
        jmp     or_condition
or_body:
        movl    (%rdi,%rcx,4),%r8d      # x_buffer = x[i]
        movl    (%rsi,%rcx,4),%r9d      # y_buffer = y[i]
        orl     %r9d,%r8d               # x_buffer = x_buffer | y_buffer
        movl    %r8d,(%rdx,%rcx,4)      # xoy[i] = buffer
or_condition:
        cmpl    $128,%ecx               # i < 128 we operate in 32 bits
        jb      or_body

        popq    %r9
        popq    %r8
        popq    %rcx
        ret
	
// BigIntXor: xxy = x ^ y	
// void BigIntXor(BigInt x, BigInt y, BigInt xxy);
// ARGUMENTS
// rdi = x
// rsi = y_buffer
// rdx = xay
// LOCALS
// rcx = i
// r8  = x_buffer
// r9  = y_buffer
BigIntXor:
	pushq   %rcx
        pushq   %r8
        pushq   %r9

        movl    $0,%ecx         # i = 0 
        jmp     xor_condition
xor_body:
        movl    (%rdi,%rcx,4),%r8d      # x_buffer = x[i]
        movl    (%rsi,%rcx,4),%r9d      # y_buffer = y[i]
        xorl    %r9d,%r8d               # x_buffer = x_buffer ^ y_buffer
        movl    %r8d,(%rdx,%rcx,4)      # xxy[i] = buffer
xor_condition:
        cmpl    $128,%ecx               # i < 128 we operate in 32 bits
        jb      xor_body

        popq    %r9
        popq    %r8
        popq    %rcx
        ret
	

// BigIntEq: returns x == y	
// int BigIntEq(BigInt x, BigInt y);
// rdi = x
// rsi = y
// rcx = i 
// r8  = x_buffer 
// r9  = y_buffer
// rax = equals_flag
BigIntEq:
	xorq	%rax,%rax
        pushq   %rcx
        pushq   %r8
        pushq   %r9

        movl    $0,%ecx         # i = 0
        movl    $1,%eax         # equals_flag = 1
        jmp     eq_condition
eq_body:
        movl    (%rdi,%rcx,4),%r8d      # x_buffer = x[i]
        movl    (%rsi,%rcx,4),%r9d      # y_buffer = y[i]
        cmpl    %r8d,%r9d               # x == y ?
        je      is_equal
        movb    $0,%al                  # equals_flag = 0
        jmp     equals_return           # quit the loop
is_equal:
        incl    %ecx                    # i ++
eq_condition:
        cmpl    $128,%ecx               # i < 128
        jb      eq_body                 
equals_return:
        popq    %r9
        popq    %r8
        popq    %rcx
	ret

// BigIntLT: returns x < y	
// int BigIntLT(BigInt x, BigInt y);
// rdi = x
// rsi = y
// rcx = i 
// r8  = x_buffer 
// r9  = y_buffer
// rax = LT_flag
BigIntLT:
	xorq	%rax,%rax
        pushq   %rcx
        pushq   %r8
        pushq   %r9

        movl    $127,%ecx        # i = 127
        movl    $1,%eax         # LT_flag = 1
        jmp     LT_condition
LT_body:
        movl    (%rdi,%rcx,4),%r8d      # x_buffer = x[i]
        movl    (%rsi,%rcx,4),%r9d      # y_buffer = y[i]
        cmpl    %r8d,%r9d               # x < y ?
        jl      is_LT
        movb    $0,%al                  # LT_flag = 0
        jmp     LT_return               # quit the loop
is_LT:
        decl    %ecx                    # i --
LT_condition:
        cmpl    $0,%ecx                 # i >= 0
        jge      LT_body                 
LT_return:
        popq    %r9
        popq    %r8
        popq    %rcx
        ret 

// BigIntGT: returns x > y	
// int BigIntGT(BigInt x, BigInt y);
// rdi = x
// rsi = y
// rcx = i 
// r8  = x_buffer 
// r9  = y_buffer
// rax = GT_flag
BigIntGT:
	xorq	%rax,%rax
        pushq   %rcx
        pushq   %r8
        pushq   %r9

        movl    $127,%ecx               # i = 127
        movl    $1,%eax                 # GT_flag = 1
        jmp     GT_condition
GT_body:
        movl    (%rdi,%rcx,4),%r8d      # x_buffer = x[i]
        movl    (%rsi,%rcx,4),%r9d      # y_buffer = y[i]
        cmpl    %r8d,%r9d               # x > y ?
        jg      is_GT
        movb    $0,%al                  # LT_flag = 0
        jmp     GT_return               # quit the loop
is_GT:
        decl    %ecx                    # i --
GT_condition:
        cmpl    $0,%ecx                 # i >= 0
        jge     GT_body                 
GT_return:
        popq    %r9
        popq    %r8
        popq    %rcx
        ret 

// BigIntDiv: xdy = x / y	
// void BigIntDiv(BigInt x, BigInt y, BigInt xdy);
// rdi = x
// rsi = y
// rdx = xdy
// r8  = BigInt mod
// r9  = x_size
// r10 = y_size
// r11 = x_negative
// r12 = y_negative
// r13 = addres_aux
// r14 = xdy_bufffer
// NOTE: this is an implementation of the long division algorithm
BigIntDiv:
	pushq   %r8
        pushq   %r9
        pushq   %r10
        pushq   %r11
        pushq   %r12
        pushq   %r13
        pushq   %r14
        movb    $0,%r11b        # set negative = 0
        # treat negative cases
        call IsBigIntNeg        # test if x is negative
        cmpl    $0,%eax         # check the return
        je      x_is_positive
        movb    %al,%r11b       # set the negative flag
        call    BigIntNeg       # x = ~x
x_is_positive:
        pushq   %rdi
        movq    %rsi,%rdi       # x = y
        call IsBigIntNeg        # test if y is negative
        cmpl    $0,%eax         # check the return
        je      y_is_positive
        movb    %al,%r12b       # set the negative flag
        call    BigIntNeg       # y = ~y
y_is_positive:        
        popq    %rdi
        # make a big int local: mod
        subq    $512,%rsp       # allocate 512 bytes for BigInt mod in the stack
        movq    (%rsp),%r8      # get the effective address of mod
        pushq   %rdi
        pushq   %rsi
        movq    %rdi,%rsi            
        movq    %r8,%rdi
        call    BigIntAssign    # mod = x (fully)
        popq    %rsi
        popq    %rdi
        # calculate the size in bits of x and y
        pushq   %rdi
        movq    %rsi,%rdi               
        call    GetBigIntSizeInBits     # get the size in bits of the dividend
        movl    %eax,%r10d              # catch the return
        popq    %rdi

div_body:
        subl    %r10d,%r9d              # calculate the shift of the dividend
        pushq   %rdi
        pushq   %rsi
        # We will shift the dividend as in long division algorithm
        movq    %rsi,%rdi               # argument BigInt y
        movl    %r9d,%esi               # argument num_shifts
        call    BigIntShl               # y << num_shifts
        # this is to check if the shifted dividend can fit in mod
        movq    %r8,%rsi                # argument mod
        call    BigIntGT                # y > mod
        cmpl    $1,%eax                 # check the return
        jne     dividend_LT_mod
        # Case he cannot: check if we can get mod an extra bit or if it is the end
        cmpl    $0,%r9d                 # num_shifts > 0
        je      div_return
        # shifts dividend right, effectively giving mod an extra bit
        decl    %r9d                    # num_shifts --
        movl    $1,%esi                 # argument num_shifts
        call    BigIntShar              # y >>
        popq    %rsi
dividend_LT_mod:        
        # this is subtraction of the long division 
        pushq   %rdx
        movq    %r8,%rdi                # argument mod
        movq    %r8,%rdx                # argument destination
        call    BigIntSub               # mod = mod - y                            
        popq    %rdx
        popq    %rdi
        # this is to store the coeficcient in xdy
        movl    %r9d,%r13d              # aux = num_shifts
        shrl    $3,%r9d                 # to get the index in bytes
        movb    (%rdx,%r9),%r14b        # xdy_buffer = byte of interest  
        andl    $0x07,%r13d             # aux = num_shifts % 8
        movb    $1,%al                  # bit_buffer
        
        pushq   %rcx
        movb    %r13b,%cl               # shl must be used with cl
        shlb    %cl,%al                 # so we can set the right bit
        addb    %al,%r14b               # so we dont lose info
        movb    %r14b,(%rdx,%r9)        # xdy [num_shifts] = xdy_buffer
        popq    %rcx
        # we get the remaining size in mod to see if we can make another step in the division
div_cond:        
        pushq   %rdi
        movq    %r8,%rdi
        call    GetBigIntSizeInBits     # get the size in bits of mod
        movl    %eax,%r9d               # catch the return
        popq    %rdi
        cmpl    %r10d,%r9d              # size_mod >= size_y        
        jb      div_body        
        # restore temporaries, dealoccate mod, undo negations
div_return:        
        cmpb    $0,%r11b
        je      xdiv_signal_restored
        call    BigIntNeg       # x = ~x
xdiv_signal_restored:   
        cmpb    $0,%r12b
        je      ydiv_signal_restored
        pushq   %rdi
        movq    %rsi,%rdi
        call    BigIntNeg       # y = ~y
        popq    %rdi
ydiv_signal_restored:
        addq    $512,%rsp
        popq    %r14
        popq    %r13
        popq    %r12
        popq    %r11
        popq    %r10
        popq    %r9
        popq    %r8
        ret   

// verifies if a BigInt is negative
// rdi = x
// r8  = buffer
// r9  = addres_aux
// rax = return flag 
IsBigIntNeg:
        pushq   %r8
        movl    $0,%eax         # flag = 0
        movl    $511,%r9d       # set address_aux
        movb    (%rdi,%r9),%r8b # get the most significant byte in x
        testb   $0x08,%r8b      # if it is 1, we want to set the flag at 1
        setne   %al             # when the test does not reduce to zero, set
        popq    %r8
        ret


// Returns the size in bits of a BigInt
// rdi  = x 
// r8   = size in bits
// r9   = byte_aux
GetBigIntSizeInBits:
        pushq   %r8
        pushq   %r9
        movl    $512,%r8d               # i = max size
        jmp     get_size_condition
get_size_body:
        decl    %r8d                    # i--
get_size_condition:
        cmpl    $0,%r8d        
        je      get_size_return         # end_consume if r10 < 0
        movb    -1(%rdi,%r8),%r9b 
        cmpb    $0,%r9b                 # while(n[i] == 0)
        je      get_size_body
        
        shll    $3,%r8d                 # to get the size in bits
        jmp     calculate_bits_cond
calculate_bits_body:
        shlb    %r9b                    # shift the last byte left
        decl    %r8d
calculate_bits_cond:
        testb   $0x080,%r9b             # until we got an significant bit
        je      calculate_bits_body
get_size_return:        
        movl    %r8d,%eax               # set return
        popq    %r8
        popq    %r9
        ret





// BigIntMod: xmy = x % y	
// void BigIntMod(BigInt x, BigInt y, BigInt xmy);
// rdi = x
// rsi = y
// rdx = xdy
// r8  = xmy_bufffer
// r9  = x_size
// r10 = y_size
// r11 = x_negative
// r12 = y_negative
// r13 = addres_aux
// r14 = xmy_bufffer
// NOTE: this is an implementation of the long division algorithm
BigIntMod:
	pushq   %r8
        pushq   %r9
        pushq   %r10
        pushq   %r11
        pushq   %r12
        pushq   %r13
        pushq   %r14
        movb    $0,%r11b        # set negative = 0
        # treat negative cases
        call IsBigIntNeg        # test if x is negative
        cmpl    $0,%eax         # check the return
        je      xmod_is_positive
        movb    %al,%r11b       # set the negative flag
        call    BigIntNeg       # x = ~x
xmod_is_positive:
        pushq   %rdi
        movq    %rsi,%rdi       # x = y
        call    IsBigIntNeg     # test if y is negative
        cmpl    $0,%eax         # check the return
        je      ymod_is_positive
        movb    %al,%r12b       # set the negative flag
        call    BigIntNeg       # y = ~y
ymod_is_positive:        
        popq    %rdi
        # calculate the size in bits of x and y
        pushq   %rdi
        movq    %rsi,%rdi               
        call    GetBigIntSizeInBits     # get the size in bits of the dividend
        movl    %eax,%r10d              # catch the return
        popq    %rdi
        # Assign x to xmy  
        pushq   %rdi
        pushq   %rsi
        movq    %rdi,%rsi            
        movq    %rbx,%rdi
        call    BigIntAssign            # xmy = x (fully)
        popq    %rsi
        popq    %rdi
mod_body:
        subl    %r10d,%r9d              # calculate the shift of the dividend
        pushq   %rdi
        pushq   %rsi
        # We will shift the dividend as in long division algorithm
        movq    %rsi,%rdi               # argument BigInt y
        movl    %r9d,%esi               # argument num_shifts
        call    BigIntShl               # y << num_shifts
        # this is to check if the shifted dividend can fit in mod
        movq    %rdx,%rsi                # argument mod
        call    BigIntGT                # y > mod
        cmpl    $1,%eax                 # check the return
        jne     mod_dividend_LT_mod
        # Case he cannot: check if we can get mod an extra bit or if it is the end
        cmpl    $0,%r9d                 # num_shifts > 0
        je      mod_return
        # shifts dividend right, effectively giving mod an extra bit
        decl    %r9d                    # num_shifts --
        movl    $1,%esi                 # argument num_shifts
        call    BigIntShar              # y >>
        popq    %rsi
mod_dividend_LT_mod:        
        # this is subtraction of the long division 
        pushq   %rdx
        movq    %rdx,%rdi                # argument mod
        movq    %rdx,%rdx                # argument destination
        call    BigIntSub               # mod = mod - y                            
        popq    %rdx
        popq    %rdi
        # we get the remaining size in mod to see if we can make another step in the division
mod_cond:        
        pushq   %rdi
        movq    %r8,%rdi
        call    GetBigIntSizeInBits     # get the size in bits of mod
        movl    %eax,%r9d               # catch the return
        popq    %rdi
        cmpl    %r10d,%r9d              # size_mod >= size_y        
        jb      mod_body        
        # restore temporaries, dealoccate mod
mod_return:        
        cmpb    $0,%r11b
        je      xmod_signal_restored
        call    BigIntNeg       # x = ~x
xmod_signal_restored:   
        cmpb    $0,%r12b
        je      ymod_signal_restored
        pushq   %rdi
        movq    %rsi,%rdi
        call    BigIntNeg       # y = ~y
        popq    %rdi
ymod_signal_restored:
        addq    $512,%rsp
        popq    %r14
        popq    %r13
        popq    %r12
        popq    %r11
        popq    %r10
        popq    %r9
        popq    %r8
        ret 
