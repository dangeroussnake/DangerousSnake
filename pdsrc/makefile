all: ncurses.mod libfncurses.a
	
macros.o: macros.c
	gcc -c macros.c  # build auxiliary C routines
	
ncurses.o: ncurses.f90 # actually creates ncurses.mod as well
	gfortran -J. -c ncurses.f90
	
ncurses.mod: ncurses.f90 # actually creates ncurses.o as well
	gfortran -J. -c ncurses.f90

libfncurses.a: macros.o ncurses.o
	ar cr libfncurses.a *.o	# build fortran curses lib

.PHONY: clean
clean:
	rm -f *.o *.a *.mod
