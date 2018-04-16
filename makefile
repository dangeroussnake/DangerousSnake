.PHONY: all
all: snake

snake: constants.f90 util.f90 bindings.f90 snake.f90 game.f90 main.f90 pdsrc
	gfortran -Wpedantic -Wall -I./pdsrc constants.f90 util.f90 bindings.f90 snake.f90 \
		game.f90 main.f90 -L./pdsrc -lfncurses -lncurses -o snake
	chmod +x snake

bindings.mod: bindings.f90
	gfortran bindings.f90 -c

.PHONY: pdsrc
pdsrc:
	make -C ./pdsrc all

.PHONY: clean
clean:
	rm -f snake *.mod
