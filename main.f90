program main
    use ncurses
    use snake
    use bindings
    implicit none

    logical, parameter :: debug = .FALSE.

    integer :: seed
    logical :: readCharacter = .TRUE.
    integer :: ierr, ikey
    integer :: i
    !0 is no exit, 1 is intentional exit, 2 is game over, 3 is win
    integer :: mexit = 0
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
    call init_game()
    call init_snake(player, .TRUE.)
    call set_pos_center(player)
    do i=1, SIZE(snakes)
        call init_snake(snakes(i), .FALSE.)
        call respawn_AI_snake(snakes(i))
    end do
    do
        if (mexit /= 0) exit
        readCharacter = .TRUE.
        do while (readCharacter)
            readCharacter = .FALSE.
            ikey = getch()
            select case(ikey)
            case(SKEY_LEFT)
                call turn_left(player)
            case(SKEY_RIGHT)
                call turn_right(player)
            case(SKEY_EXIT)
                mexit = 1
            case(SKEY_ADVANCE)
            case(ERR) !do nothing
            case default
                !remove useless characters from input buffer
                readCharacter = .TRUE.
            end select
        end do
        if(move_snake(player) /= 0) then
            mexit = 2
        end if
        if(player%bodyLen+1 >= maxBody) then
            mexit = 3
        end if
        call display_snake(player)
        do i=1, SIZE(snakes)
            call advance_AI(snakes(i))
        end do
        !draw
        do i=1, SIZE(snakes)
            call display_snake(snakes(i))
        end do
        call draw_info(player, debug)
        ierr = refresh()
        ierr = wrefresh(field)
        call usleep(get_sleep_time_us(player%bodyLen, .TRUE.))
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
