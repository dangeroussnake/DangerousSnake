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
    ierr = curs_set(0)
    call srand(TIME())

    call setup_colors()
    
    do
        call show_menu(exit, mode)
        if(exit) then
            exit
        else
            select case(mode)
                case(MODE_SNAKE, MODE_SNAKE_AI)
                    call run_game(mode, debug)
                case(MODE_SCORES)
                    call show_scores()
            end select
        end if
    end do

    ierr = endwin()
end program main
