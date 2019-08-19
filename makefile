# compiler
FC = gfortran

# common Fortran compile flags
FCFLAGS = -Wpedantic -Wall -I./pdsrc
# common Fortran linker flags
FLFLAGS = -L./pdsrc -lfncurses -lncurses

# debug flags
DEBUGFLAGS += -g -O0
# release flags
RELEASEFLAGS += -O2

# filename of the game
TARGET = snake
SRC_DIR = src
BUILD_DIR = build
DEBUG_BUILD_DIR = debug
RELEASE_BUILD_DIR = release
SOURCES = constants.f90 util.f90 bindings.f90 snake.f90 game.f90 main.f90


# targets
debug: FCFLAGS += $(DEBUGFLAGS)
debug: $(TARGET)

$(TARGET): $(SOURCES) pdsrc
	$(FC) $(FCFLAGS) $(SOURCES) $(FLFLAGS) -o $(TARGET) 
	chmod +x $(TARGET)

release: FCFLAGS += $(RELEASEFLAGS)
release: $(TARGET)

pdsrc:
	make -C ./pdsrc all

.PHONY: clean
clean:
	rm -f snake *.mod *.o
