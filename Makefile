64-bit:
	gcc -m64 -g -c runtime.c -o runtime.o
	gcc -m64 -g -c scheme_utils.c -o scheme_utils.o
	as --64 -g output.s -o output.o
	gcc -m64 -g -o program output.o runtime.o scheme_utils.o

32-bit:
	gcc -m32 -g -c runtime.c -o runtime.o
	gcc -m32 -g -c scheme_utils.c -o scheme_utils.o
	as --32 -g output.s -o output.o
	gcc  -m32 -g -o program output.o runtime.o scheme_utils.o

.PHONY: reset clean objrm
reset : clean
	rm program
clean :
	rm *.[os]
objrm :
	rm *.o
