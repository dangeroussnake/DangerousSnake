module game
    use ncurses
    use snake
    use bindings
    use constants
    use util
    implicit none
contains
    subroutine show_menu(mexit, mode)
        logical, intent(out) :: mexit
        integer, intent(out) :: mode
        character(len=11), dimension(0:3) :: choices = &
            [ "Snake      ", "Snake w/ AI", "Scores     ", "Exit       "]
        integer :: ch, ierr, choice = 0
        type(C_PTR)         :: menu_win
        integer             :: x=2
        integer             :: y
        integer             :: i
        menu_win = stdscr
        ierr=keypad(stdscr, TRUE)
        ierr = wclear(menu_win)
        do
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
        ierr = nodelay(stdscr, logical(.TRUE., 1))
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
        if(mode == MODE_SNAKE) call save_score(player%bodyLen)
        select case(mexit)
        case(EXIT_GAME_OVER)
            ierr = attron(COLOR_PAIR(6))
            ierr = mvprintw(1, mwMaxX/2 - 5, "Game over!"//C_NULL_CHAR)
            ierr = attroff(COLOR_PAIR(6))
            call wait_for_exit()
        case(EXIT_WIN)
            ierr = attron(COLOR_PAIR(6))
            ierr = mvprintw(1, mwMaxX/2 - 4, "You won!"//C_NULL_CHAR)
            ierr = attroff(COLOR_PAIR(6))
            call wait_for_exit()
        end select
        ierr = delwin(field)
        ierr = nodelay(stdscr, logical(.FALSE., 1))
    end subroutine run_game

    subroutine wait_for_exit()
        integer :: ikey
        integer :: ierr
        ierr = nodelay(stdscr, logical(.FALSE., 1))
        do while (.TRUE.)
            ikey = getch()
            if (ikey==SKEY_EXIT) exit
        end do
    end subroutine wait_for_exit

    function load_scores() result(scores)
        integer, dimension(10,2) :: scores
        integer :: ioStat
        integer :: score, count
        integer :: i = 1

        open(20, file=SCORES_FILE, iostat=ioStat)
        if(ioStat == 0) then
            do
                read(20, fmt='(2I10)', iostat=ioStat) score, count
                if(ioStat /= 0) exit
                scores(i,1) = score
                scores(i,2) = count
                i = i + 1
            end do
            close(20)
        end if
        do i=i, 10
            scores(i,1) = -1
            scores(i,2) = -1
        end do
    end function load_scores

    subroutine write_scores(scores)
        integer, intent(in), dimension(10,2) :: scores
        integer :: ioStat, i
        open(20, file=SCORES_FILE)
        if(ioStat == 0) then
            do i=1, size(scores,1)
                if(scores(i,1) <= -1 .and. scores(i,2) <= -1) exit
                write(20, fmt="(2I10)", iostat=ioStat) scores(i,1), scores(i,2)
                if(ioStat /= 0) exit
            end do
        end if
    end subroutine write_scores

    subroutine show_scores()
        integer, dimension(10,2) :: scores
        integer :: ierr, ch
        integer :: y = 1
        integer :: i
        scores = load_scores()
        ierr = wclear(stdscr)
        ierr = box(stdscr, 0_C_LONG, 0_C_LONG)
        ierr = mvprintw(y, 2, "SCORES"//C_NULL_CHAR)
        y = y + 1
        do i=1, size(scores,1)
            if(scores(i,1) <= -1 .and. scores(i,2) <= -1) exit
            ierr = mvprintw(y+i, 2, trim(to_string(scores(i,1),3))//" "&
                //trim(to_string(scores(i,2),3))//"x"//C_NULL_CHAR)
        end do
        ierr = wrefresh(stdscr)
        ch = wgetch(stdscr)
    end subroutine show_scores

    subroutine save_score(score)
        integer, intent(in) :: score
        integer, dimension(10,2) :: scores
        integer, dimension(2) :: buf, tmp
        integer :: i
        logical :: shift = .FALSE.
        scores = load_scores()
        do i=1, size(scores,1)
            if(.not. shift) then
                if(score == scores(i,1)) then
                    scores(i,2) = scores(i,2) + 1
                    exit
                else if(score > scores(i,1)) then
                    if(i==1) then
                        shift = .TRUE.
                        buf(1) = score
                        buf(2) = 1
                    else
                        if(score < scores(i-1,1)) then
                            shift = .TRUE.
                            buf(1) = score
                            buf(2) = 1
                        end if
                    end if
                end if
            end if
            if(shift) then
                tmp = buf
                buf = scores(i,:)
                scores(i,:) = tmp
            end if
        end do
        call write_scores(scores)
    end subroutine save_score
end module game
