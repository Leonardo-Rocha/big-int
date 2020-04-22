        .section       .data
        
fmt:    .asciz "number: %d\n"
base:   .asciz "base: %d\n"
BigInt: .fill 512              # n[512] = {0}

        .text
        .globl _start, NumberToChar
        .extern printf
 
_start: 
        # movq $BigInt,%rdi # n = BigInt
        # movq $10,%rsi     # b = 10

        # call BigIntRead   # BigIntRead(BigInt, 10);

        movq    $65,%rdi
        call    CharToNumber

        movq    $fmt,%rdi
        movq    %rax,%rsi
        xorq    %rax,%rax
        call    printf 

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


        movq $60,%rax    # exit syscall
        movq $0,%rdi     # return value: exit(0)
        syscall

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
