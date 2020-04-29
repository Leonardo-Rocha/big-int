        .section       .data
BigInt: .fill 512               # n[512] = {0}
BigInt2:.fill 512
BigInt3:.fill 512
BigIntA:.asciz "4444444444444444"
BigIntB:.asciz "1111111111111111"
test_neg: .asciz "\nBigIntNeg(15) = \n"
test_shl: .asciz "\nBigIntShl(15) = \n"
test_shr: .asciz "\nBigIntShr(15) = \n"
test_or:  .asciz "\nBigIntOr(15,3) = \n"
test_xor: .asciz "\nBigIntXor(15,15) = \n"
test_and: .asciz "\nBigIntAnd(15,3) = \n"
test_eq:  .asciz "\nBigIntEq(3,3) = %d\n"
test_gt:  .asciz "\nBigIntGT(3,15) = %d\n"
test_lt:  .asciz "\nBigIntLT(3,15) = %d\n"
test_mul: .asciz "\nBigIntMul(3,15) = \n"
test_sub: .asciz "\nBigIntSub(15,45) = \n"
test_div: .asciz "\nBigIntDiv(-30,15) = \n"
test_mod: .asciz "\nBigIntMod(15,-2) = \n"

        .text
        .globl _start, BigIntRead, BigIntPrint, BigIntToStr
        .globl CharToNumber, Log2, BigIntScale_by10, BigIntDiv10
        .globl NumberToChar, CalculateReadReverseCounter, _BigIntRead
        .globl CalculateWriteReverseCounter
        .globl BigIntAdd, BigIntNeg, BigIntSub, BigIntShl, BigIntShar
        .globl BigIntMul, BigIntAssign, BigIntMod, BigIntDiv
        .globl IsBigIntNeg, GetBigIntSizeInBits
        .globl BigIntEq, BigIntGT, BigIntLT, BigIntXor, BigIntOr, BigIntAnd, ConsumeStringZeros
        .extern printf
 
_start: 
        movq    $BigInt,%rdi # n = BigInt
        movq    $10,%rsi     
        movq    $BigIntA,%rdx
        movq    $17,%rcx
        call    _BigIntRead   # BigIntRead(BigInt, 2);

        movq    $BigInt2,%rdi # n = BigInt
        movq    $10,%rsi      
        movq    $BigIntB,%rdx
        movq    $17,%rcx
        call    _BigIntRead   # BigIntRead(BigInt, 2);

        movq    $BigInt,%rdi
        movq    $10,%rsi
        call    BigIntPrint

        movq    $BigInt2,%rdi
        movq    $10,%rsi
        call    BigIntPrint

        # Test unary operations
        movq    $BigInt,%rdi
        call    BigIntNeg

        movq    $test_neg,%rdi
        xorl    %eax,%eax
        call    printf

        movq    $BigInt,%rdi
        movq    $10,%rsi
        call    BigIntPrint
        
        movq    $BigInt,%rdi
        call    BigIntNeg

        # Test shifts
        movq    $test_shl,%rdi
        xorl    %eax,%eax
        call    printf
        
        movq    $BigInt,%rdi
        movq    $3,%rsi
        call    BigIntShl

        movq    $BigInt,%rdi
        movq    $10,%rsi
        call    BigIntPrint

        movq    $test_shr,%rdi
        xorl    %eax,%eax
        call    printf

        movq    $BigInt,%rdi
        movq    $3,%rsi
        call    BigIntShar

        movq    $BigInt,%rdi
        movq    $10,%rsi
        call    BigIntPrint
        
        # Test Logical operations
        movq    $BigInt,%rdi
        movq    $BigInt2,%rsi
        movq    $BigInt2,%rdx
        call    BigIntOr

        movq    $test_or,%rdi
        xorl    %eax,%eax
        call    printf

        movq    $BigInt2,%rdi
        movq    $10,%rsi
        call    BigIntPrint
        
        movq    $BigInt,%rdi
        movq    $BigInt2,%rsi
        movq    $BigInt2,%rdx
        call    BigIntXor

        movq    $test_xor,%rdi
        xorl    %eax,%eax
        call    printf

        movq    $BigInt2,%rdi
        movq    $10,%rsi
        call    BigIntPrint

        movq    $BigInt2,%rdi # n = BigInt
        movq    $10,%rsi      
        movq    $BigIntB,%rdx
        movq    $17,%rcx
        call    _BigIntRead   # BigIntRead(BigInt, 2);

        movq    $test_and,%rdi
        xorl    %eax,%eax
        call    printf

        movq    $BigInt,%rdi
        movq    $BigInt2,%rsi
        movq    $BigInt2,%rdx
        call    BigIntAnd

        movq    $BigInt2,%rdi
        movq    $10,%rsi
        call    BigIntPrint

        movq    $BigInt2,%rdi # n = BigInt
        movq    $10,%rsi      
        movq    $BigIntB,%rdx
        movq    $17,%rcx
        call    _BigIntRead   # BigIntRead(BigInt, 2);
        
        # Test Comparators
        movq    $BigInt2,%rdi
        movq    $BigInt2,%rsi
        call    BigIntEq
        
        movq    $test_eq,%rdi
        movq    %rax,%rsi
        xorl    %eax,%eax
        call    printf

        movq    $BigInt2,%rdi
        movq    $BigInt,%rsi
        call    BigIntGT

        movq    $test_gt,%rdi
        movq    %rax,%rsi
        xorl    %eax,%eax
        call    printf

        movq    $BigInt,%rsi
        movq    $BigInt2,%rdi
        call    BigIntLT

        movq    $test_lt,%rdi
        movq    %rax,%rsi
        xorl    %eax,%eax
        call    printf
        
        # Test Multiplication
        movq    $BigInt2,%rdi
        movq    $BigInt,%rsi
        movq    $BigInt2,%rdx
        call    BigIntMul

        movq    $test_mul,%rdi
        xorl    %eax,%eax
        call    printf

        movq    $BigInt2,%rdi
        movq    $8,%rsi
        call    BigIntPrint

        movq    $test_sub,%rdi
        xorl    %eax,%eax
        call    printf

        # Test Sub
        movq    $BigInt,%rdi
        movq    $BigInt2,%rsi
        movq    $BigInt2,%rdx
        call    BigIntSub
        
        movq    $BigInt2,%rdi
        movq    $10,%rsi
        call    BigIntPrint

        # Test Div
        movq    $BigInt2,%rdi
        movq    $BigInt,%rsi
        movq    $BigInt2,%rdx
        call    BigIntDiv

        movq    $test_div,%rdi
        xorl    %eax,%eax
        call    printf

        movq    $BigInt2,%rdi
        movq    $10,%rsi
        call    BigIntPrint

        movq    $test_mod,%rdi
        xorl    %eax,%eax
        call    printf

        movq    $BigInt,%rdi
        movq    $BigInt2,%rsi
        movq    $BigInt2,%rdx
        call    BigIntMod

        movq    $BigInt2,%rdi
        movq    $10,%rsi
        call    BigIntPrint

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
// ARGUMENTS
// rdi = n[]; rsi = base; 
// LOCAL REGISTERS
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
// ARGUMENTS
// rdi = n[]; rsi = base; rdx = buffer; rcx = size
// LOCAL REGISTERS
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
        pushq   %rbp 
        pushq   %rbx
        pushq   %r8
        pushq   %r9
        pushq   %r10
        pushq   %r11 
        pushq   %r12 
        pushq   %r13
        pushq   %r14
        pushq   %r15

        call BigIntZero         # immediately zero the BigInt

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
        movq    %rsp,%r15
        pushq   %rdi
        movq    %r15,%rdi
        call    BigIntZero
        popq    %rdi
        movq    $4294967296,%r14
        movq    %r14,(%rsp)     # two_p_32BigInt = 2^32
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
        jnc     no_overflow           
        
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
        popq    %rbp
        ret

// void BigIntScale_by10(BigInt n, int numBytes)
// ARGUMENTS
// rdi = BigInt n[], rsi = numBytes 
// LOCAL REGISTERS
// r12 = n[k]
// r13 = temp
// r14 = i
// r15 = temp
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
// ARGUMENTS
// rdi = n[]; rsi = base;
// LOCAL REGISTERS
// rbx = BigIntToStr
// r9 = i
// r8 = n[i]
BigIntPrint:
        pushq	%rbp
        pushq   %rbx 
        pushq   %rcx
        pushq   %rdx   
        pushq   %r8
        pushq   %r9
        subq    $4104,%rsp

        movq    %rsi,%rdx
        movq    %rsp,%rsi
        call    BigIntToStr
        movq    %rax,%rbx           
        
        xorl    %r9d,%r9d               # i = 0
print:  
        movq    $1,%rax
        movq    $1,%rdi
        movq    %rbx,%rsi
        addq    %r9,%rsi
        movq    $1,%rdx
        syscall                         # sys_write(stdout, rsp + i, 1)
        cmpl    $-1,%eax
        je      print_return
        incl    %r9d                    # t9++
print_condition:
        movb    (%rbx,%r9),%r8b         # t8 = n[i]
        cmpb    $0,%r8b                 # while n[i] != '\0'
        jne     print
        
        movq    $10,(%rbx,%r9)         # print '\n'
        movq    $1,%rax
        movq    $1,%rdi
        movq    %rbx,%rsi
        addq    %r9,%rsi
        movq    $1,%rdx
        syscall                         # sys_write(stdout, rsp + i, 1)
        
        movl    %r9d,%eax               # ret = t9
print_return:
        # free the stack
        addq	$4104,%rsp
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
// ARGUMENTS
// rdi = n, rsi = *buf, rdx = b
// LOCAL REGISTERS
// rbx = n[] 
// r8 = b (base - 2, 8, 10, 16)
// r9 = log2(base) // decimal_copy
// r10 = i
// r11 = reverse_counter
BigIntToStr:
        pushq   %rbp
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
        je      end_of_cz               # if r10 == 0
        movb    (%rbx,%r10),%dl 
        cmpb    $0,%dl                  # while(n[i] == 0)
        je      consume_zeros_body
end_of_cz:        
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
        xorq    %r12,%r12
        jmp     to_string_return

decimal_case:
        incl    %r10d

        subq    $512,%rsp             # allocate 512 Bytes in Stack for a BigIntCopy
        movq    %rsp,%r9              # get the address of the local BigIntCopy
        
        # NOTE: calculation of correct max decimal chars might be unecessary, considering zeros are already trimmed in print 
        movl    %r10d,%r11d
        shll    $2,%r11d                # max decimal chars (insn't exact, only a superior limit)
        cmpl    $1237,%r11d             # max_decimal_chars > 1236
        jbe     in_string_limit
        movl    $1237,%r11d               
in_string_limit:
        movl    %r11d,%r13d             # string_end = size
        movl    $0, %r12d               # inferior string limit
        pushq   %rdi
        pushq   %rsi
        movq    %r9,%rdi                # argument destination                
        movq    %rbx,%rsi               # argument source
        call    BigIntAssign
        movq    %r9,%rbx                # now rbx->copy
        
        movq    %rbx,%rdi
        call    IsBigIntNeg
        cmpl    $0,%eax                 # check return
        je      not_neg
        call    BigIntNeg       
        movl    $1,%r12d                # set is neg flag   
        # movb    $48,(%rbx)              # buf[0] = '0'     
not_neg:
        popq    %rsi
        popq    %rdi
        
        jmp     decimal_condition
decimal_body:
        pushq   %rsi
        movq    %rbx,%rdi               # argument BigInt x
        movl    %r10d,%esi              # argument numBytes (*)
        call    BigIntDiv10             # returns n % 10 in eax, and leave the BigInt divided by 10;
        popq    %rsi
        movl    %eax,%edi               # argument num
        call    NumberToChar            # NumberToChar(num)
        movb    %al,-1(%rsi,%r11)       # buf[m] = char
        decl    %r11d                   # m--
decimal_condition:
        cmpl    %r12d,%r11d               
        jg      decimal_body            # end if r11 < 0
        addq    $512,%rsp               # deallocate the BigIntCopy 

to_string_return:          
        movb    $0,(%rsi,%r13)          # add '\0' in the end of the string
        movq    %rsi,%rax               # ret = buffer
        
        movq    %rsi,%rdi
        movq    %r13,%rsi
        movq    %r12,%rdx 
        call    ConsumeStringZeros
        
        popq    %r14 
        popq    %r13 
        popq    %r12 
        popq    %r11 
        popq    %r10 
        popq    %r9 
        popq    %r8 
        popq    %rbx 
        popq    %rcx 
        popq    %rbp
        ret

// ConsumeStringZeros( char * string, int size, int isNeg)
// ARGUMENTS
// rdi = string
// rsi = size
// rdx = is neg
// LOCAL REGISTERS
// rcx = j
// r8  = char_buffer
// r9  = address_aux
ConsumeStringZeros:
        # do not print zeros on the left.
        pushq   %rcx
        pushq   %r8
        pushq   %r9
        decl    %esi
        xorl    %ecx,%ecx
        addl    %edx,%ecx
        jmp     consume_string_cond
consume_string_body: 
        incl    %ecx                    # i++
consume_string_cond:
        cmpl    %esi,%ecx               # i < size
        je      end_of_consume
        movb    (%rdi,%rcx),%r8b        # edx = n[i]
        cmpb    $48,%r8b                # while(n[i] == '0')
        je      consume_string_body      
end_of_consume:
        addl    $2,%esi    
        # e se n zerarmos ecx e começarmos a partir do ecx
        movl    $0,%r9d                 # address_aux = 0
        cmpl    $1,%edx                 # if is neg, we shift one less
        jne     not_negative_st
        movb    $45,(%rdi)              # buf[m] = '-'    
        movl    $1,%r9d   
not_negative_st:        
        jmp     shift_char_cond
shift_char_body:                        
        movb    (%rdi,%rcx),%r8b        # buffer = n[i]
        movb    %r8b,(%rdi,%r9)         # n[i - shift] = buffer 
        incl    %r9d                    # address++
        incl    %ecx                    # i++
shift_char_cond:
        cmpl    %esi,%ecx
        jb      shift_char_body
consume_string_return: 
        popq    %r9
        popq    %r8
        popq    %rcx
        ret

// int BigIntDiv10(BigInt n, int numBytes)
// ARGUMENTS
// Bigint n = %rdi, numBytes = %rsi
// LOCAL REGISTERS
// r8 = n[i]
// r9 = quociente
// r10 = mod result
// rdx = magic number
// eax = Bigint % 10         
BigIntDiv10: 
        pushq   %rbp            # store frame
        pushq   %rdx
        pushq   %r8             # store temporary
        pushq   %r9             # store temporary
        pushq   %r10            # store temporary
        xorl    %r10d,%r10d
        shrl    %esi         # to get size in words
        incl    %esi
        jmp     DIV_COND
DIV_BODY:        
        xorq    %r8,%r8                 # make sure q=0
        movw    -2(%rdi,%rsi,2),%r8w    # q = n[size]
        movl	$1717986919, %edx       # magic number
	shll    $16,%r10d               # to raise it's significance
        addl    %r10d,%r8d
        movl	%r8d,%eax
	imull	%edx                    # eax * edx = [rdx:rax] 128 bits
	sarl	$2, %edx
	movl	%r8d,%eax
	sarl	$31,%eax
	subl	%eax,%edx
	movl	%edx,%eax               # div_result
	movl    %eax,%r9d
        movw    %r9w,-2(%rdi,%rsi,2)    # n[size] = q
        # mod calculation
        sall	$2,%eax
	addl	%edx,%eax
	addl	%eax,%eax
	subl	%eax,%r8d
	movl	%r8d,%r10d              # mod result
        decl    %esi                    # size--
DIV_COND:
        cmpl    $0,%esi                 # size == 0
        jg      DIV_BODY
DIV_RETURN:
        movl    %r10d,%eax      # return r ; r = Bigint % 10
        popq    %r10            # restore temporary
        popq    %r9             # restore temporary
        popq    %r8             # restore temporary
        popq    %rdx
        popq    %rbp            # restore frame
        ret

// Size must be a number of characters
// int CalculateReadReverseCounter(int size, int log2Base)
// ARGUMENTS
// %rdi = size, %rsi = log2Base
// LOCAL REGISTERS
// rdx = higher 64 bits of mul
// r13 = temp
CalculateReadReverseCounter:       
        pushq   %rdx            
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
        popq    %rdx 
        ret

// Size must be a number of bits
// int CalculateWriteReverseCounter(int size, int log2Base)
// ARGUMENTS
// %rdi = size, %rsi = log2Base
// LOCAL REGISTERS
// ecx = temp
// r13 = temp
CalculateWriteReverseCounter:   
        pushq   %r13                 
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
// ARGUMENTS
// rdi = x; rsi = y; rdx = xmy	
// LOCAL REGISTERS
// r8 = local xmy
BigIntSub:
        pushq   %rbp 
        pushq   %r8
        subq    $512,%rsp 

        movq    %rsp,%r8        # local xmy = new BigInt
        
        pushq   %rdi
        movq    %r8,%rdi
        call    BigIntAssign    # local xmy = y (fully)
        
        movq    %r8,%rdi       
        call    BigIntNeg       # local xmy = -y
        popq    %rdi
        
        pushq   %rsi
        movq    %r8,%rsi 
        call    BigIntAdd       # xmy = x + local xmy = x + (-y)
        popq    %rsi

        addq    $512,%rsp
        popq    %r8
        popq    %rbp   
        ret

// BigIntAdd: xpy = x + y				
// void BigIntAdd(BigInt x, BigInt y, BigInt xpy);	
// ARGUMENTS
// rdi = x; rsi = y; rdx = xpy
// LOCAL REGISTERS
// rcx = i
// r8 = x[i]
// r9 = y[i]
// r10 = xpy[i]
// r11 = carry
// r12 = carry_temp
BigIntAdd:
        pushq   %rcx
        pushq   %r8
        pushq   %r9
        pushq   %r10
        pushq   %r11
        pushq   %r12
        xorl    %ecx,%ecx         # i = 0
        xorl    %r8d,%r8d         # x[i]          1...1111110001
        xorl    %r9d,%r9d         # y[i]          0...0000101101
        xorl    %r10d,%r10d       # xpy[i]        0...0000011110
        xorl    %r11d,%r11d       # carry
        xorl    %r12d,%r12d
add_body:
        movl    (%rdi,%rcx,4),%r8d      # update x=x[i]
        movl    (%rsi,%rcx,4),%r9d      # update y=y[i]
        addl    %r11d,%r9d              # y += carry
        setb    %r12b
        addl    %r8d,%r9d               # y = x+y
        setb    %r11b                   # carry flag
        movl    %r9d,%r10d              # xpy = x+y
        movl    %r10d,(%rdx,%rcx,4)     # xpy[i] = xpy
        addb    %r12b,%r11b
        incl    %ecx                    # i++
add_cond:
        cmpl    $128,%ecx               # i < 128       we operate 32 bits at a time
        jb      add_body     
        
        popq    %r12           
        popq    %r11
        popq    %r10
        popq    %r9
        popq    %r8
        popq    %rcx
        ret

// BigIntNeg: x = ~x		
// void BigIntNeg(BigInt x);
// ARGUMENTS
// rdi = x
// LOCAL REGISTERS
// rcx = i
// r8 = x[i]
// r9 = carry
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
// LOCAL REGISTERS
// rcx = i
// r8  = num_shifts
// r9  = y_buffer
// r10 = address_aux / buffer_aux
// r11 = internal_counter : j
// r12 = xty local copy
BigIntMul:
	pushq   %rbp
        pushq   %rcx
        pushq   %r8
        pushq   %r9
        pushq   %r10
        pushq   %r11
        pushq   %r12
        subq    $512,%rsp
        movq    %rsp,%r12       # r_12 = new BigInt

        movl    $0,%ecx         # i = 0
        movl    $0,%r8d         # num_shifts = 1

        pushq   %rsi
        movq    %rdi,%r10       # aux = x (address)
        movq    %r12,%rdi       # x = x_copy (address)
        movq    %r10,%rsi       # y = aux (address)
        call    BigIntAssign    # x_copy = x (fully)
        popq    %rsi            # restore y address

        subq    $512,%rsp
        movq    %rsp,%r12       # r12 = xty local copy

        pushq   %rdi
        movq    %r12,%rdi
        call    BigIntZero      # xty = {0}
        popq    %rdi
        # from this moment on rdi -> x_copy
        jmp     mul_condition
mul_body:
        movl    (%rsi,%rcx,4),%r9d      # y_buffer = y[i]
        xorl    %r11d,%r11d             # j = 0
        jmp     mul_intern_condition
mul_intern_body:
        testl   $0x01,%r9d              # verifies if last bit is positive
        je      no_shift
        pushq   %rsi                    # stores y address
        pushq   %rdx
        movl    %r8d,%esi               # argument num_shifts
        call    BigIntShl               # xty << num_shifts if last bit is positive
        movq    %r12,%rsi               # xty += x << num_shifts
        movq    %r12,%rdx
        call    BigIntAdd
        xorl    %r8d,%r8d
        popq    %rdx
        popq    %rsi                    # restores y address
no_shift:
        incl    %r8d                    # get the next num_shifts value
        shrl    %r9d                    # y_buffer >> ; so we can check the next bit
        incl    %r11d                   # j++
mul_intern_condition:
        cmpl    $32,%r11d               # j < 32
        jb      mul_intern_body
        incl    %ecx                    # i++
mul_condition:
        cmpl    $128,%ecx               # i < 128
        jb      mul_body
        
        movq    %rdx,%rdi
        movq    %r12,%rsi
        call    BigIntAssign            # xty = xty local copy

        addq    $1024,%rsp
        popq    %r12
        popq    %r11
        popq    %r10
        popq    %r9
        popq    %r8
        popq    %rcx
        popq    %rbp
        ret     

// BigIntAssign: x = y				
// void BigIntAssign(BigInt x, BigInt y);
// ARGUMENTS
// rdi = x
// rsi = y
// LOCAL REGISTERS
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
        incl    %ecx                    # i++
assign_condition:
        cmpl    $128,%ecx               # i < 128 we operate in 32 bits
        jb      assign_body

        popq    %r8
        popq    %rcx
        ret

// BigIntShl:   bi = bi << num_shifts
// ARGUMENTS
// rdi: BigInt bi
// rsi: int num_shifts
// LOCAL REGISTERS
// rcx: current_shifts
// r8 : bi_buffer
// r9 : internal iterator : i
// r10: prev_shift_letfover
// r11: temp
BigIntShl:
        pushq   %rcx
        pushq   %r8
        pushq   %r9
        pushq   %r10                
        pushq   %r11
        jmp     shl_condition
shl_body:
        movl    %esi,%ecx               # current_shifts = num_shifts
        cmpl    $32,%ecx
        jb      in_shl_limit
        movl    $31,%ecx
in_shl_limit:
        subl    %ecx,%esi               # calculate remaining shifts
        movl    $0,%r9d                 # i = 0
        movl    $0,%r10d                # prev_shift_leftover = 0
        jmp     shl_intern_condition
shl_intern_body:
        movl    (%rdi,%r9,4),%r8d       # bi_buffer = bi[i] ;32 bits at time 
        shlq    %cl,%r8                 # bi_buffer << current_shifts
        addq    %r10,%r8              # bi_buffer += prev_shift_leftover
        movq    %r8,%r11
        movl    %r8d,(%rdi,%r9,4)       # bi[i] = bi[i] << current_shifts
        shrq    $16,%r11
        shrq    $16,%r11                # cleans the attributed 32 bits
        movl    %r11d,%r10d             # store the leftover
        incl    %r9d                    # i++
shl_intern_condition:
        cmpl    $128 ,%r9d              # i < 128
        jb      shl_intern_body
shl_condition:
        cmpl    $0,%esi                 # num_shifts > 0
        jg      shl_body

        popq    %r11
        popq    %r10
        popq    %r9
        popq    %r8
        popq    %rcx
        ret

// BigIntShar:   bi = bi << num_shifts
// ARGUMENTS
// rdi: BigInt bi
// rsi: int num_shifts
// LOCAL REGISTERS
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
        cmpl    $32,%ecx
        jb      in_shar_limit
        movl    $31,%ecx
in_shar_limit:
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
        movl    %r11d,%r10d             # get the next leftover
        shll    %r10d                   # fix to complete 1 + 31 = 32
        decl    %r9d                    # i++
shar_intern_condition:
        cmpl    $0,%r9d                  # i > 0
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
// LOCAL REGISTERS
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
        incl    %ecx                    # i++
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
// LOCAL REGISTERS
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
        incl    %ecx                    # i++
or_condition:
        cmpl    $128,%ecx               # i < 128 we operate in 32 bits
        jb      or_body

        popq    %r9
        popq    %r8
        popq    %rcx
        ret
	
// BigIntZero: n[i] = 0, 0 <= i <= 512
// void BigIntZero(BigInt n)
// ARGUMENTS
// rdi = n
// LOCAL REGISTERS
// rcx = i
BigIntZero:
        pushq   %rcx
        movl    $0,%ecx         # i =0
        jmp     zero_condition
zero_body:
        movl    $0,(%rdi,%rcx,4) # n[i] = 0
        incl    %ecx
zero_condition:
        cmpl    $128,%ecx       # i < 128
        jb      zero_body
        popq    %rcx
        ret

// BigIntXor: xxy = x ^ y	
// void BigIntXor(BigInt x, BigInt y, BigInt xxy);
// ARGUMENTS
// rdi = x
// rsi = y
// rdx = xxy
// LOCAL REGISTERS
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
        incl    %ecx                    # i++
xor_condition:
        cmpl    $128,%ecx               # i < 128 we operate in 32 bits
        jb      xor_body

        popq    %r9
        popq    %r8
        popq    %rcx
        ret

// BigIntEq: returns x == y	
// int BigIntEq(BigInt x, BigInt y);
// ARGUMENTS
// rdi = x
// rsi = y
// LOCAL REGISTERS
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
        cmpl    %r9d,%r8d               # x == y ?
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
// ARGUMENTS
// rdi = x
// rsi = y
// LOCAL REGISTERS
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
        cmpl    %r9d,%r8d               # x < y ?
        jle      is_LT
        movb    $0,%al                  # LT_flag = 0
        jmp     LT_return               # quit the loop
is_LT:
        decl    %ecx                    # i --
LT_condition:
        cmpl    $0,%ecx                 # i >= 0
        jge      LT_body    
        cmpl    %r9d,%r8d               # x > y ?
        setne   %al             
LT_return:
        popq    %r9
        popq    %r8
        popq    %rcx
        ret 

// BigIntGT: returns x > y	
// int BigIntGT(BigInt x, BigInt y);
// ARGUMENTS
// rdi = x
// rsi = y
// LOCAL REGISTERS
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
        cmpl    %r9d,%r8d              # x > y ?
        jge     is_GT
        movb    $0,%al                  # LT_flag = 0
        jmp     GT_return               # quit the loop
is_GT:
        decl    %ecx                    # i --
GT_condition:
        cmpl    $0,%ecx                 # i >= 0
        jge     GT_body
        cmpl    %r9d,%r8d               # x > y ?
        setne   %al
GT_return:
        popq    %r9
        popq    %r8
        popq    %rcx
        ret 

// BigIntDiv: xdy = x / y	
// void BigIntDiv(BigInt x, BigInt y, BigInt xdy);
// ARGUMENTS
// rdi = x
// rsi = y
// rdx = xdy
// LOCAL REGISTERS
// r9  = x_size
// r10 = y_size
// r11 = x_negative
// r12 = y_negative
// r13 = addres_aux
// r14 = xdy_bufffer
// NOTE: this is an implementation of the long division algorithm
BigIntDiv:
        pushq   %rbp
        pushq   %rcx
        pushq   %r8
        pushq   %r9
        pushq   %r10
        pushq   %r11
        pushq   %r12
        pushq   %r13
        pushq   %r14
        
        xorl    %r11d,%r11d        # set negative = 0
        xorl    %r12d,%r12d        # set negative = 0
        
        # make 2 big int locals: mod and dividend
        subq    $512,%rsp       # allocate 512 bytes for BigInt mod in the stack
        movq    %rsp,%r8        # get the effective address of mod
        pushq   %rsi
        movq    %rdi,%rsi            
        movq    %r8,%rdi
        call    BigIntAssign    # mod = x (fully)
        popq    %rsi
        # rdi -> mod
        subq    $512,%rsp       # allocate 512 bytes for BigInt mod in the stack
        movq    %rsp,%r8        # get the effective address of mod
        pushq   %rdi            
        movq    %r8,%rdi
        call    BigIntAssign    # dividend = y (fully)
        movq    %r8,%rsi
        # rsi -> dividend 
        movq    %rdx,%rdi 
        call    BigIntZero
        popq    %rdi
        # treat negative cases
        call    IsBigIntNeg     # test if mod is negative
        cmpl    $0,%eax         # check the return
        je      x_is_positive
        addb    %al,%r11b       # set the negative flag
        movq    %rdi,%rdi
        call    BigIntNeg       # mod = ~mod
x_is_positive:
        pushq   %rdi
        movq    %rsi,%rdi       # x = divdend
        call    IsBigIntNeg     # test if divdend is negative
        cmpl    $0,%eax         # check the return
        je      y_is_positive
        addb    %al,%r11b       # set the negative flag
        call    BigIntNeg       # divdend = ~divdend
y_is_positive:      
        popq    %rdi
        # calculate the size in bits of x and y
        pushq   %rdi
        movq    %rsi,%rdi               
        call    GetBigIntSizeInBits     # get the size in bits of the dividend
        movl    %eax,%r10d              # catch the return
        popq    %rdi
        jmp     div_cond
div_body:
        subl    %r10d,%r9d              # calculate the shift of the dividend
        pushq   %rdi
        pushq   %rsi
        # We will shift the dividend as in long division algorithm
        movq    %rsi,%rdi               # argument BigInt divdend
        movl    %r9d,%esi               # argument num_shifts
        call    BigIntShl               # divdend << num_shifts
        # this is to check if the shifted dividend can fit in mod
        popq    %rsi
        popq    %rdi
        
        pushq   %rdi
        pushq   %rsi
        call    BigIntLT        # mod < divdend
        popq    %rsi
        popq    %rdi            
        cmpl    $1,%eax                 # check the return
        jne     dividend_LT_mod
        # Case he cannot: check if we can get mod an extra bit or if it is the end
        cmpl    $0,%r9d                 # num_shifts > 0
        je      div_return
        # shifts dividend right, effectively giving mod an extra bit
        decl    %r9d                    # num_shifts --
        pushq   %rdi
        pushq   %rsi
        movq    %rsi,%rdi               # argument divdend
        movl    $1,%esi                 # argument num_shifts
        call    BigIntShar              # divdend >>
        popq    %rsi
        popq    %rdi
dividend_LT_mod:        
        
        # this is subtraction of the long division 
        pushq   %rdi
        pushq   %rsi
        pushq   %rdx
        movq    %rdi,%rdx                # argument destination
        call    BigIntSub               # mod = mod - divdend                            
        popq    %rdx
        popq    %rsi
        popq    %rdi
        # this is to store the coeficcient in xdy
        movl    %r9d,%r13d              # aux = num_shifts
        pushq   %r9
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
        # we need to return dividend to its original position
        popq    %r9
        pushq   %rdi
        pushq   %rsi
        # We will shift back the dividend as in long division algorithm
        movq    %rsi,%rdi               # argument BigInt divdend
        movl    %r9d,%esi               # argument num_shifts
        call    BigIntShar              # divdend >> num_shifts
        popq    %rsi
        popq    %rdi
        
        # we get the remaining size in mod to see if we can make another step in the division
div_cond:        
        pushq   %rdi
        call    GetBigIntSizeInBits     # get the size in bits of mod
        movl    %eax,%r9d               # catch the return
        popq    %rdi
        cmpl    %r10d,%r9d              # size_mod >= size_y        
        jge     div_body        
        # restore temporaries, dealoccate mod, undo negations
div_return:
        testb   $0x01,%r11b
        je      not_neg_out
        pushq   %rdi    
        movq    %rdx,%rdi
        call    BigIntNeg
        popq    %rdi        
not_neg_out:
        movq    %rdi,%rax
        addq    $1024,%rsp
        popq    %r14
        popq    %r13
        popq    %r12
        popq    %r11
        popq    %r10
        popq    %r9
        popq    %r8
        popq    %rcx
        popq    %rbp
        ret   

// verifies if a BigInt is negative
// ARGUMENTS
// rdi = x
// LOCAL REGISTERS
// r8  = buffer
// r9  = addres_aux
// rax = return flag 
IsBigIntNeg:
        pushq   %r8
        pushq   %r9 
        movl    $0,%eax         # flag = 0
        movl    $511,%r9d       # set address_aux
        movb    (%rdi,%r9),%r8b # get the most significant byte in x
        testb   $0x08,%r8b      # if it is 1, we want to set the flag at 1
        setne   %al             # when the test does not reduce to zero, set
        popq    %r9
        popq    %r8
        ret

// Returns the size in bits of a BigInt
// ARGUMENTS
// rdi  = x 
// LOCAL REGISTERS
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
        popq    %r9
        popq    %r8
        ret

// BigIntMod: xmy = x % y	
// void BigIntMod(BigInt x, BigInt y, BigInt xmy);
// ARGUMENTS
// rdi = x
// rsi = y
// rdx = xdy
// LOCAL REGISTERS
// r8  = x_neg
// r9  = y_neg
// NOTE: this is an implementation of the long division algorithm
BigIntMod:
       call     BigIntDiv
       pushq    %rdi
       pushq    %rsi
       movq     %rdx,%rdi 
       movq     %rax,%rsi
       call     BigIntAssign
       popq     %rsi
       popq     %rdi
       call     IsBigIntNeg
       cmpl     $0,%eax
       je       mod_ret
       pushq    %rdi
       pushq    %rsi
       movq     %rsi,%rdi
       call     IsBigIntNeg
       cmpl     $0,%eax
       je       mod_ret
       movq     %rdx,%rdi
       call     BigIntNeg
       popq     %rsi
       popq     %rdi 
mod_ret:
       ret
