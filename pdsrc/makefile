.PHONY: all
all: ncurses.mod libfncurses.a
	
macros.o:
	gcc -c macros.c  # build auxiliary C routines
	
ncurses%mod ncurses%o:	# build two targets from the same rule
	gfortran -J. -c ncurses.f90
	
libfncurses.a: macros.o ncurses.o
	ar cr libfncurses.a *.o	# build fortran curses lib

.PHONY: clean
clean:
	rm -f *.o *.a *.mod
