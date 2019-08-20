# PDSRC library
include(FortranCInterface)
FortranCInterface_VERIFY()

add_library(${PDSRC_LIB} STATIC "")

target_sources(${PDSRC_LIB} 
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/macros.c
    PUBLIC
    ${CMAKE_CURRENT_LIST_DIR}/ncurses.f90
)

# ncurses
find_package(Curses REQUIRED)
include_directories(${CURSES_INCLUDE_DIR})
target_link_libraries(${PDSRC_LIB} 
    PUBLIC
        ${CURSES_LIBRARIES}
    )