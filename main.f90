program main
    use ncurses
    use snake
    use bindings
    implicit none

    integer :: seed
    logical :: debug = .FALSE.
    logical :: readCharacter = .TRUE.
    integer :: ierr, ikey
    !0 is no exit, 1 is intentional exit, 2 is game over, 3 is win
    integer :: mexit = 0
    integer(C_LONG) :: ch
    seed = TIME()
    stdscr = initscr()
    ierr = start_color()
    ierr = cbreak()
    ierr = noecho()
    ierr = nodelay(stdscr, logical(.TRUE., 1))
    ierr = curs_set(0)
    call srand(seed)
    call setup_colors()
    call getmaxyx(stdscr, mwMaxY, mwMaxX)
    field = newwin(mwMaxY - headerHeight, mwMaxX, headerHeight, 0);
    ierr = wbkgd(stdscr, COLOR_PAIR(5))
    ierr = wbkgd(field, COLOR_PAIR(4))
    call init()
    do
        if (mexit /= 0) exit
        readCharacter = .TRUE.
        do while (readCharacter)
            readCharacter = .FALSE.
            ikey = getch()
            select case(ikey)
            case(SKEY_LEFT)
                call turn_left()
            case(SKEY_RIGHT)
                call turn_right()
            case(SKEY_EXIT)
                mexit = 1
            case(ERR) !do nothing
            case default
                !remove useless characters from input buffer
                readCharacter = .TRUE.
            end select
        end do
        call draw_info(debug)
        call move_snake(mexit)
        ierr = refresh()
        ierr = wrefresh(field)
        call usleep(get_sleep_time_us(bodyLen, .TRUE.))
        if (boostTicks > 0) boostTicks = boostTicks - 1
    end do
    select case(mexit)
    case(2)
        ierr = attron(COLOR_PAIR(6))
        ierr = mvprintw(1, mwMaxX/2 - 5, "Game over!" // C_NULL_CHAR)
        ierr = attroff(COLOR_PAIR(6))
        ierr = nodelay(stdscr, logical(.FALSE., 1))
        call wait_for_exit()
    case(3)
        ierr = attron(COLOR_PAIR(6))
        ierr = mvprintw(1, mwMaxX/2 - 4, "You won!" // C_NULL_CHAR)
        ierr = attroff(COLOR_PAIR(6))
        ierr = nodelay(stdscr, logical(.FALSE., 1))
        call wait_for_exit()
    end select
    ierr = delwin(field)
    ierr = endwin()
end program main

subroutine wait_for_exit()
  use ncurses
  use snake
  implicit none
  integer :: ikey
  do while (.TRUE.)
    ikey = getch()
    if (ikey==SKEY_EXIT) exit
  end do
end subroutine wait_for_exit
