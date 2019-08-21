# CMake project file for DangerousSnake

##################################################
# Define the project and the depencies that it has
##################################################

cmake_minimum_required(VERSION 3.5)
project(DANGEROUS_SNAKE LANGUAGES Fortran C)

# Set the DangerousSnake version
set(VERSION 0.1.0)

# Add our local modlues to the module path
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Add a distclean target to the Makefile
add_custom_target(distclean 
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_MODULE_PATH}/distclean.cmake
)

# Uncomment if it is required that Fortran 90 is supported
if(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    message(FATAL_ERROR "Fortran compiler does not support F90")
endif(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)

# Set some options the user may choose
# Uncomment the below if you want the user to choose a parallelization library
#OPTION(USE_MPI "Use the MPI library for parallelization" OFF)
#OPTION(USE_OPENMP "Use OpenMP for parallelization" OFF)

# This INCLUDE statement executes code that sets the compile flags for DEBUG,
# RELEASE, and TESTING. You should review this file and make sure the flags 
# are to your liking.
include(${CMAKE_MODULE_PATH}/SetFortranFlags.cmake) 
# Locate and set parallelization libraries. There are some CMake peculiarities
# taken care of here, such as the fact that the FindOpenMP routine doesn't know
# about Fortran.
#INCLUDE(${CMAKE_MODULE_PATH}/SetParallelizationLibrary.cmake)

# There is an error in CMAKE with this flag for pgf90.  Unset it
get_filename_component(FCNAME ${CMAKE_Fortran_COMPILER} NAME)
if(FCNAME STREQUAL "pgf90")
    UNSET(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS)
endif(FCNAME STREQUAL "pgf90")

############################################################
# Define the actual files and folders that make up the build
############################################################

# Define the executable name
set(DANGEROUS_SNAKE_EXE snake)

# Define the library name
set(PDSRC_LIB ncurses)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin)
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_SOURCE_DIR}/modules)

# Add source directories
add_subdirectory(src)


