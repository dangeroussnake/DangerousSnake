program main
    use ncurses
    use game
    implicit none

    logical, parameter :: debug = .FALSE.

    integer :: ierr
    integer :: mode
    logical :: exit

    stdscr = initscr()
    ierr = start_color()
    ierr = cbreak()
    ierr = noecho()
    ierr = nodelay(stdscr, logical(.TRUE., 1))
    ierr = curs_set(0)
    call srand(TIME())

    call show_menu(exit, mode)
    if(.not. exit) then
        call run_game(mode, debug)
    end if

    ierr = endwin()
end program main
