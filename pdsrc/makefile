all:
	gcc -c macros.c  # build auxiliary C routines
	gfortran -J. -c ncurses.f90
	ar cr libfncurses.a *.o

clean:
	rm -f *.o *.a *.mod
