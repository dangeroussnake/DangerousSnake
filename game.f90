module game
    use ncurses
    use snake
    use bindings
    use constants
    implicit none
contains
    subroutine show_menu(mexit, mode)
        logical, intent(out) :: mexit
        integer, intent(out) :: mode
        character(len=11), dimension(0:2) :: choices = &
            [ "Snake      ", "Snake w/ AI", "Exit       "]
        integer :: ch, ierr, choice = 0
        type(C_PTR)         :: menu_win
        integer             :: x=2
        integer             :: y
        integer             :: i
        menu_win = stdscr
        ierr=keypad(stdscr, TRUE)
        do
            ch = wgetch(menu_win)
            select case(ch)
            case(KEY_UP)
                choice = modulo(choice-1,size(choices))
            case(KEY_DOWN)
                choice = modulo(choice+1,size(choices))
            case(SKEY_ENTER)
                if(choice == size(choices)-1) then
                    mode = -1
                else
                    mode = choice
                    mexit = .FALSE.
                end if
                exit
            case(SKEY_ESCAPE, SKEY_EXIT)
                mode = -1
                exit
            end select

            !display menu
            y = 2
            ierr=box(menu_win, 0_C_LONG, 0_C_LONG)
            do i=0,size(choices)-1,1
              if(choice == i) then
                 ierr = wattron(menu_win, A_REVERSE)
                 ierr = mvwaddstr(menu_win, y, x, trim(choices(i))//C_NULL_CHAR)
                 ierr = wattroff(menu_win, A_REVERSE)
              else
                 ierr = mvwaddstr(menu_win, y, x, choices(i)//C_NULL_CHAR)
              endif
              y = y + 1
            end do
            ierr = wrefresh(menu_win)
        end do
        if(mode == -1) then
            mexit = .TRUE.
        end if
    end subroutine show_menu

    subroutine run_game(mode, debug)
        integer, intent(in) :: mode
        logical, intent(in) :: debug
        logical :: readCharacter = .TRUE.
        integer :: ierr, ikey
        integer :: i
        integer :: mexit = EXIT_NONE
        call setup_colors()
        call getmaxyx(stdscr, mwMaxY, mwMaxX)
        ierr = wclear(stdscr)
        field = newwin(mwMaxY - headerHeight, mwMaxX, headerHeight, 0);
        ierr = wbkgd(stdscr, COLOR_PAIR(5))
        ierr = wbkgd(field, COLOR_PAIR(4))
        call init_game(mode)
        call init_snake(player, .TRUE.)
        call set_pos_center(player)
        do i=1, aiCount
            call init_snake(snakes(i), .FALSE.)
            call respawn_AI_snake(snakes(i))
        end do
        do
            if (mexit /= EXIT_NONE) exit
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
                    mexit = EXIT_MANUAL
                case(SKEY_ADVANCE)
                case(ERR) !do nothing
                case default
                    !remove useless characters from input buffer
                    readCharacter = .TRUE.
                end select
            end do
            if(move_snake(player) /= COLLISION_NONE) then
                mexit = EXIT_GAME_OVER
            end if
            if(player%bodyLen+1 >= maxBody) then
                mexit = EXIT_WIN
            end if
            call display_snake(player)
            do i=1, aiCount
                call advance_AI(snakes(i))
            end do
            !draw
            do i=1, aiCount
                call display_snake(snakes(i))
            end do
            call draw_info(player, debug)
            ierr = refresh()
            ierr = wrefresh(field)
            call usleep(get_sleep_time_us(player%bodyLen, .TRUE.))
            if (boostTicks > 0) boostTicks = boostTicks - 1
        end do
        select case(mexit)
        case(EXIT_GAME_OVER)
            ierr = attron(COLOR_PAIR(6))
            ierr = mvprintw(1, mwMaxX/2 - 5, "Game over!" // C_NULL_CHAR)
            ierr = attroff(COLOR_PAIR(6))
            ierr = nodelay(stdscr, logical(.FALSE., 1))
            call wait_for_exit()
        case(EXIT_WIN)
            ierr = attron(COLOR_PAIR(6))
            ierr = mvprintw(1, mwMaxX/2 - 4, "You won!" // C_NULL_CHAR)
            ierr = attroff(COLOR_PAIR(6))
            ierr = nodelay(stdscr, logical(.FALSE., 1))
            call wait_for_exit()
        end select
        ierr = delwin(field)
    end subroutine run_game

    subroutine wait_for_exit()
        integer :: ikey
        do while (.TRUE.)
            ikey = getch()
            if (ikey==SKEY_EXIT) exit
        end do
    end subroutine wait_for_exit
end module game
