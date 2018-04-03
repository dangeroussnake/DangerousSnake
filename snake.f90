module snake
    use ncurses
    implicit none

    !CONSTANTS
    integer, parameter :: headerHeight = 4
    integer, parameter :: foodAmount = 3
    integer, parameter :: boostTime_ms = 3000
    real, parameter :: boostIntensity = 0.6
    integer, parameter :: maxBody = 200
    integer, parameter :: aiCount = 5
    integer, parameter :: aiMaxBodyLen = 15

    integer, parameter :: SKEY_EXIT = ichar("q",8)
    integer, parameter :: SKEY_LEFT = ichar("a",8)
    integer, parameter :: SKEY_RIGHT = ichar("d",8)
    integer, parameter :: SKEY_ADVANCE = ichar(" ",8)

    !TYPES
    type :: Point
        integer :: x, y
    end type Point

    type :: Snake_t
        !0: up, 1: right, 2: down, 3: left
        integer :: direction
        type(Point) :: head
        type(Point), dimension(maxBody) :: body
        integer :: bodyLen
        integer :: idleTicks
        logical :: resetOnIdleEnd
        logical :: is_player
    end type Snake_t


    !VARIABLES
    !max x and y of the main window
    integer :: mwMaxX, mwMaxY
    !max x and y of the subwindow used as playfield
    integer :: fieldMaxX, fieldMaxY
    integer :: boostTicks = 0
    type(C_PTR) :: field
    !player's snake
    type(Snake_t) :: player
    !ki's snakes
    type(Snake_t), dimension(aiCount) :: snakes


contains

    subroutine init_game()
        type(Snake_t) :: this
        integer :: i
        call getmaxyx(field, fieldMaxY, fieldMaxX)
        !align field
        fieldMaxX = fieldMaxX - MODULO(fieldMaxX, 2)
        call set_pos_center(this)
        do i = 1, foodAmount
            call generate_food()
        end do
    end subroutine init_game

    subroutine init_snake(this, is_player)
        type(Snake_t) :: this
        logical :: is_player
        integer :: i
        this%bodyLen = 1
        this%direction = 1
        this%idleTicks = -1
        this%resetOnIdleEnd = .FALSE.
        this%is_player = is_player
        do i = 1, maxBody
            this%body(i) = Point(-1,-1)
        end do
    end subroutine init_snake

    !@return 0: no collision, 1: ai->ai collision, 2: ai->player collision
    function move_snake(this) result(collision)
        type(Snake_t) :: this
        integer :: collision
        integer :: i
        integer :: new_x, new_y
        integer(chtype) :: cell
        new_x = this%head%x
        new_y = this%head%y

        select case(this%direction)
        case(0)
            new_y = MODULO(this%head%y - 1, fieldMaxY)
        case(1)
            new_x = MODULO(this%head%x + 2, fieldMaxX)
        case(2)
            new_y = MODULO(this%head%y + 1, fieldMaxY)
        case(3)
            new_x = MODULO(this%head%x - 2, fieldMaxX)
        end select

        !move body
        do i = this%bodyLen + 1, 2, -1
            this%body(i) = this%body(i - 1)
        end do

        this%body(1) = this%head

        !move head
        this%head%x = new_x
        this%head%y = new_y

        !check collision
        cell = mvwinch(field, new_y, new_x)
        if (iand(cell, A_CHARTEXT) == ichar("*")) then
            !on eat
            call eat_food(this)
            collision = 0
            return
        else if (iand(cell, A_CHARTEXT) /= ichar(" ")) then
            if(iand(cell, A_COLOR) == COLOR_PAIR(7) &
                .or. iand(cell, A_COLOR) == COLOR_PAIR(8)) then
                collision = 2
            else
                collision = 1
            end if
            return
        else
            collision = 0
            return
        end if
    end function move_snake

    subroutine display_snake(this)
        type(Snake_t) :: this
        integer :: i
        integer(C_LONG) :: ierr
        integer :: headColor, bodyColor

        if(this%is_player) then
            headColor = 7
        else
            headColor = 1
        end if
        if(this%idleTicks>-1) then
            bodyColor = 1
        else if(this%is_player) then
            bodyColor = 8
        else
            bodyColor = 2
        end if
        !render
        ierr = wattron(field, COLOR_PAIR(headColor))
        ierr = mvwaddch(field, this%head%y, this%head%x, ichar("X", 8))
        ierr = wattroff(field, COLOR_PAIR(headColor))

        ierr = wattron(field, COLOR_PAIR(bodyColor))
        do i = 1, this%bodyLen
            if(this%body(i)%x == -1 .or. this%body(i)%y == -1) exit
            ierr = mvwaddch(field, this%body(i)%y, this%body(i)%x, ichar("0", 8))
        end do
        ierr = wattroff(field, COLOR_PAIR(bodyColor))
        ierr = mvwaddch(field, this%body(this%bodyLen + 1)%y, &
            this%body(this%bodyLen + 1)%x, ichar(" ", 8))
    end subroutine display_snake

    subroutine advance_AI(this)
        type(Snake_t) :: this
        integer :: res
        if(this%idleTicks>0) then
            this%idleTicks = this%idleTicks - 1
            return
        else if(this%idleTicks==0) then
            this%idleTicks = -1
            if(this%resetOnIdleEnd .eqv. .TRUE.) then
                call clear_snake(this)
                call respawn_AI_snake(this)
            end if
        end if
        select case(MODULO(irand(), 10))
        case (0)
            call turn_left(this)
        case (1)
            call turn_right(this)
        end select
        res = move_snake(this)
        if(res /= 0) then
            this%idleTicks = 5
            this%resetOnIdleEnd = .TRUE.
            if(res == 2) then
                call eat_food(player)
            end if
        end if
    end subroutine advance_AI

    subroutine clear_snake(this)
        type(Snake_t) :: this
        integer :: i
        integer(C_LONG) :: ierr
        ierr = mvwaddch(field, this%head%y, this%head%x, ichar(" ", 8))
        this%head = Point(-1,-1)
        do i=1, this%bodyLen
            ierr = mvwaddch(field, this%body(i)%y, this%body(i)%x, ichar(" ", 8))
            this%body(i) = Point(-1,-1)
        end do
    end subroutine clear_snake

    subroutine respawn_AI_snake(this)
        type(Snake_t) :: this
        integer :: side
        side = MODULO(irand(),4)
        select case(side)
        case(0) !top side
            this%bodyLen = MODULO(irand(), aiMaxBodyLen)+1
            this%head%x = alignX(MODULO(irand(), fieldMaxX))
            this%head%y = 0
        case(1) !right side
            this%bodyLen = MODULO(irand(), aiMaxBodyLen)+1
            this%head%x = fieldMaxX
            this%head%y = MODULO(irand(), fieldMaxY+1)
        case(2) !bottom side
            this%bodyLen = MODULO(irand(), aiMaxBodyLen)+1
            this%head%x = alignX(MODULO(irand(), fieldMaxX))
            this%head%y = fieldMaxY
        case(3) !left side
            this%bodyLen = MODULO(irand(), aiMaxBodyLen)+1
            this%head%x = 0
            this%head%y = MODULO(irand(), fieldMaxY+1)
        end select
    end subroutine respawn_AI_snake

    function alignX(x)
        integer :: x
        integer :: alignX
        if (MODULO(x, 2) == 1) then
            x = MODULO(x + 1, fieldMaxX)
        end if
        alignX = x
        return
    end function alignX

    subroutine set_pos_center(this)
        type(Snake_t) :: this
        !setup and align the snake you start with
        this%head%x = alignX(fieldMaxX/2)
        this%head%y = fieldMaxY/2
        !add body
        this%body(1)%x = this%head%x - 1
        this%body(1)%y = this%head%y
    end subroutine set_pos_center

    subroutine turn_left(this)
        type(Snake_t) :: this
        this%direction = MODULO(this%direction - 1, 4)
    end subroutine turn_left

    subroutine turn_right(this)
        type(Snake_t) :: this
        this%direction = MODULO(this%direction + 1, 4)
    end subroutine turn_right

    subroutine eat_food(this)
        type(Snake_t) :: this
        this%bodyLen = this%bodyLen + 1
        if(this%is_player .eqv. .TRUE.) then
            boostTicks = INT(boostTicks + boostTime_ms/ &
                (get_sleep_time_us(this%bodyLen, .FALSE.)/1000.0))
        end if
        call generate_food()
    end subroutine eat_food

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

    subroutine draw_info(this, debug)
        type(Snake_t) :: this
        logical :: debug
        integer(C_LONG) :: ierr
        character(len=3) :: str

        ierr = move(1, 0)
        ierr = clrtoeol()
        if (.NOT. can_change_color()) then
            !draw separator at the end of the header
            ierr = mvhline(headerHeight - 1, 0, 0_C_LONG, mwMaxX)
        end if
        ierr = mvprintw(0, 0, "Press q to exit" // C_NULL_CHAR)
        ierr = mvprintw(1, 0, "Length: " // C_NULL_CHAR)
        if (boostTicks > 0) then
            ierr = attron(COLOR_PAIR(6))
            ierr = mvprintw(1, mwMaxX/2 - 4, "BOOSTING" // C_NULL_CHAR)
            ierr = attroff(COLOR_PAIR(6))
        end if
        write(str, '(i3)') this%bodyLen
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
            ierr = init_pair(7_C_SHORT, COLOR_BLUE, 9_C_SHORT)                  !player head
            ierr = init_pair(8_C_SHORT, COLOR_YELLOW, 9_C_SHORT)                !player body
        else
            ierr = init_pair(1_C_SHORT, COLOR_RED, COLOR_BLACK)                 !snake head
            ierr = init_pair(2_C_SHORT, COLOR_GREEN, COLOR_BLACK)               !snake body
            ierr = init_pair(3_C_SHORT, COLOR_YELLOW, COLOR_BLACK)              !food
            ierr = init_pair(4_C_SHORT, COLOR_WHITE, COLOR_BLACK)               !background
            ierr = init_pair(5_C_SHORT, COLOR_WHITE, COLOR_BLACK)               !default
            ierr = init_pair(6_C_SHORT, COLOR_RED, COLOR_BLACK)                 !alert text
            ierr = init_pair(7_C_SHORT, COLOR_BLUE, COLOR_BLACK)                !player head
            ierr = init_pair(8_C_SHORT, COLOR_YELLOW, COLOR_BLACK)              !player body
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
