all: snake.f90
	$(MAKE) -C ./pdsrc
	gfortran -I./pdsrc snake.f90 -L./pdsrc -lfncurses -lncurses -o snake
	chmod +x snake

clean:
	rm -f snake *.mod
