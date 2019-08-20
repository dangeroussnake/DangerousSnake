########################################
# Set up how to compile the source files
########################################

# Define the executable in terms of the source files
add_executable(${DANGEROUS_SNAKE_EXE} "")

# Add the source files
target_sources(${DANGEROUS_SNAKE_EXE} 
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/main.f90
        ${CMAKE_CURRENT_SOURCE_DIR}/constants.f90
        ${CMAKE_CURRENT_SOURCE_DIR}/game.f90
        ${CMAKE_CURRENT_SOURCE_DIR}/snake.f90
        ${CMAKE_CURRENT_SOURCE_DIR}/util.f90
        ${CMAKE_CURRENT_SOURCE_DIR}/bindings.f90
)

#####################################################
# Add the needed libraries and special compiler flags
#####################################################

# This links DANGEROUS_SNAKE to the PDSRC library
target_link_libraries(${DANGEROUS_SNAKE_EXE} ${PDSRC_LIB})

# Uncomment if you have parallization
#IF(USE_OPENMP)
#    SET_TARGET_PROPERTIES(${FOOEXE} PROPERTIES
#                          COMPILE_FLAGS "${OpenMP_Fortran_FLAGS}"
#                          LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
#ELSEIF(USE_MPI)
#    SET_TARGET_PROPERTIES(${FOOEXE} PROPERTIES
#                          COMPILE_FLAGS "${MPI_Fortran_COMPILE_FLAGS}"
#                          LINK_FLAGS "${MPI_Fortran_LINK_FLAGS}")
#    INCLUDE_DIRECTORIES(${MPI_Fortran_INCLUDE_PATH})
#    TARGET_LINK_LIBRARIES(${FOOEXE} ${MPI_Fortran_LIBRARIES})
#ENDIF(USE_OPENMP)

#####################################
# Tell how to install this executable
#####################################

if(WIN32)
    set(CMAKE_INSTALL_PREFIX "C:\\Program Files")
else()
    set(CMAKE_INSTALL_PREFIX /usr/local)
endif(WIN32)
install(TARGETS ${DANGEROUS_SNAKE_EXE} RUNTIME DESTINATION bin)