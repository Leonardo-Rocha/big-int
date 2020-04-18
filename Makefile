# Makefile for the project.
# Best viewed with tabs set to 4 spaces.

CC = gcc

BI = ./bin
BU = ./build
RC = ./src

CCOPTS = -Wall -g -c -I$(RC)

# Makefile targets

all: bitest libbi.a

libbi.a: $(BI)/libbi.o
	ar rcs $(BU)/libbi.a $(BI)/libbi.o

bitest: $(BI)/bitest.o libbi.a 
	$(CC) -L$(BU) -o $(BU)/bitest $(BI)/bitest.o -lbi

# Clean up!
clean:
	rm -f $(BI)/bitest.o $(BU)/bitest $(BI)/libbi.o $(BU)/libbi.a

# No, really, clean up!
distclean: clean
	rm -f *~
	rm -f \#*
	rm -f *.bak

# How to compile a C file
$(BI)/%.o:$(RC)/%.c
	$(CC) $(CCOPTS) -o $@ $<

# How to assemble
$(BI)/%.o:$(RC)/%.s
	$(CC) $(CCOPTS) -o $@ $<

# How to produce assembler input from a C file
$(RC)/%.s:$(RC)/%.c
	$(CC) $(CCOPTS) -S -o $@ $<
