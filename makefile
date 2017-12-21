all: snake.f90
	gfortran -I../pdsrc -J../pdsrc snake.f90 ../pdsrc/ncurses.o ../pdsrc/macros.o -lncurses -o snake
