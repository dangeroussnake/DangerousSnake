module snake
    use ncurses
    implicit none

    type :: Point
        integer :: x, y
    end type Point
    
    !CONSTANTS
    integer, parameter :: headerHeight = 4
    integer, parameter :: foodAmount = 3
    integer, parameter :: boostTime_ms = 3000
    real, parameter :: boostIntensity = 0.6
    integer, parameter :: maxBody = 200
    
    !VARIABLES
    integer :: direction
    type(Point) :: head
    type(Point), dimension(maxBody) :: body
    integer :: bodyLen
    !max x and y of the main window
    integer :: mwMaxX, mwMaxY
    !max x and y of the subwindow used as playfield
    integer :: fieldMaxX, fieldMaxY
    integer :: boostTicks = 0
    type(C_PTR) :: field

contains

    subroutine init()
        integer :: i
        bodyLen = 1
        direction = 1
        do i = 1, maxBody
            body(i) % x = -1
            body(i) % y = -1
        end do
        call getmaxyx(field, fieldMaxY, fieldMaxX)
        !align field
        fieldMaxX = fieldMaxX - MODULO(fieldMaxX, 2)
        !setup the snake you start with
        head % x = fieldMaxX/2
        head % y = fieldMaxY/2
        body(1) % x = head % x - 1
        body(1) % y = head % y
        !align snake
        if (MODULO(head % x, 2) == 1) then
            head % x = head % x + 1
        end if
        do i = 1, foodAmount
            call generate_food()
        end do
    end subroutine init

    subroutine move_snake(mexit)
        integer :: mexit, i
        integer :: new_x, new_y
        integer(C_LONG) :: ch, ierr
        new_x = head % x
        new_y = head % y

        select case(direction)
        case(0)
            new_y = MODULO(head % y - 1, fieldMaxY)
        case(1)
            new_x = MODULO(head % x + 2, fieldMaxX)
        case(2)
            new_y = MODULO(head % y + 1, fieldMaxY)
        case(3)
            new_x = MODULO(head % x - 2, fieldMaxX)
        end select

        !move body
        do i = bodyLen + 1, 2, -1
            body(i) = body(i - 1)
        end do

        body(1) = head !correct

        !move head
        head % x = new_x
        head % y = new_y

        !check collision
        ch = iand(mvwinch(field, new_y, new_x), A_CHARTEXT)
        if (ch == ichar("*")) then
            !on eat
            bodyLen = bodyLen + 1
            boostTicks = boostTicks + boostTime_ms/(get_sleep_time_us(bodyLen, .FALSE.)/1000.0)
            if (bodyLen+1 >= maxBody) then
                mexit = 3
            end if
            call generate_food()
        else if (ch /= ichar(" ")) then
            mexit = 2
            return
        end if

        !render
        ierr = wattron(field, COLOR_PAIR(1))
        ierr = mvwaddch(field, head % y, head % x, ichar("X", 8))
        ierr = wattroff(field, COLOR_PAIR(1))

        ierr = wattron(field, COLOR_PAIR(2))
        do i = 1, bodyLen
            ierr = mvwaddch(field, body(i) % y, body(i) % x, ichar("0", 8))
        end do
        ierr = wattroff(field, COLOR_PAIR(2))
        ierr = mvwaddch(field, body(bodyLen + 1) % y, body(bodyLen + 1) % x, ichar(" ", 8))
    end subroutine move_snake

    subroutine turn_left()
        direction = MODULO(direction - 1, 4)
    end subroutine turn_left

    subroutine turn_right()
        direction = MODULO(direction + 1, 4)
    end subroutine turn_right

    subroutine generate_food()
        integer :: ierr, x, y
        integer(C_LONG) :: ch

        ch = ichar(" ")
        do
            x = MODULO(irand(), fieldMaxX)
            y = MODULO(irand(), fieldMaxY)
            !align food
            x = x + MODULO(x, 2)
            if (iand(mvwinch(field, y, x), A_CHARTEXT) == ch) exit
        end do
        ierr = wattron(field, COLOR_PAIR(3))
        ierr = mvwaddch(field, y, x, ichar("*", 8))
        ierr = wattroff(field, COLOR_PAIR(3))
    end subroutine generate_food

    subroutine draw_info(debug)
        logical :: debug
        integer(C_LONG) :: ierr
        integer :: i
        character(len=3) :: str

        ierr = move(1, 0)
        ierr = clrtoeol()
        !draw separator at the end of the header
        if (.NOT. can_change_color()) then
            ierr = mvhline(headerHeight - 1, 0, 0_C_LONG, mwMaxX)
        end if
        ierr = mvprintw(0, 0, "Press q to exit" // C_NULL_CHAR)
        ierr = mvprintw(1, 0, "Length: " // C_NULL_CHAR)
        if (boostTicks > 0) then
            ierr = attron(COLOR_PAIR(6))
            ierr = mvprintw(1, mwMaxX/2 - 4, "BOOSTING" // C_NULL_CHAR)
            ierr = attroff(COLOR_PAIR(6))
        end if
        write(str, '(i3)') bodyLen
        ierr = mvprintw(1, 8, str)
    end subroutine draw_info

    subroutine setup_colors()
        integer :: ierr
        if (can_change_color()) then
            ierr = init_color(9_C_SHORT, 150_C_SHORT, 74_C_SHORT, 7_C_SHORT)    !brown
            ierr = init_color(10_C_SHORT, 1000_C_SHORT, 0_C_SHORT, 0_C_SHORT)   !red
            ierr = init_pair(1_C_SHORT, 10_C_SHORT, 9_C_SHORT)                  !snake head
            ierr = init_pair(2_C_SHORT, COLOR_GREEN, 9_C_SHORT)                 !snake body
            ierr = init_pair(3_C_SHORT, COLOR_YELLOW, 9_C_SHORT)                !food
            ierr = init_pair(4_C_SHORT, COLOR_WHITE, 9_C_SHORT)                 !background
            ierr = init_pair(5_C_SHORT, COLOR_WHITE, COLOR_BLACK)               !default
            ierr = init_pair(6_C_SHORT, 10_C_SHORT, COLOR_BLACK)                !alert text
        else
            ierr = init_pair(1_C_SHORT, COLOR_RED, COLOR_BLACK)                 !snake head
            ierr = init_pair(2_C_SHORT, COLOR_GREEN, COLOR_BLACK)               !snake body
            ierr = init_pair(3_C_SHORT, COLOR_YELLOW, COLOR_BLACK)              !food
            ierr = init_pair(4_C_SHORT, COLOR_WHITE, COLOR_BLACK)               !background
            ierr = init_pair(5_C_SHORT, COLOR_WHITE, COLOR_BLACK)               !default
            ierr = init_pair(6_C_SHORT, COLOR_RED, COLOR_BLACK)                 !alert text
        end if

    end subroutine setup_colors

    integer function get_sleep_time_us(len, applyBoost)
        implicit none
        integer, intent(in) :: len
        logical, intent(in) :: applyBoost
        integer :: sleep_time = 6000000
        real :: intensity
        if (boostTicks > 0 .AND. applyBoost) then
            intensity = boostIntensity
        else
            intensity = 1.0
        end if
        get_sleep_time_us = INT(sleep_time/(len+15) * intensity)
    end function get_sleep_time_us

end module snake
