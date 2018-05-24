module snake
    use ncurses
    use constants
    implicit none

    !TYPES
    type :: Point
        integer :: x, y
    end type Point

    type :: Snake_t
        integer :: direction
        type(Point) :: head
        type(Point), dimension(MAX_BODY_LEN) :: body
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
    integer :: boostTicks
    type(C_PTR) :: field
    !player's snake
    type(Snake_t) :: player
    !ki's snakes
    type(Snake_t), dimension(MAX_AI_COUNT) :: snakes
    integer :: aiCount
    logical :: toggle
contains

    subroutine init_game(mode)
        integer, intent(in) :: mode
        integer :: i
        select case(mode)
        case(MODE_SNAKE)
            aiCount = 0
        case(MODE_SNAKE_AI)
            aiCount = MAX_AI_COUNT
        end select
        boostTicks = 0
        toggle = .FALSE.
        call getmaxyx(field, fieldMaxY, fieldMaxX)
        !align field
        fieldMaxX = fieldMaxX - modulo(fieldMaxX, 2)
        do i = 1, FOOD_AMOUNT
            call generate_food()
        end do
    end subroutine init_game

    subroutine init_snake(this, is_player)
        type(Snake_t), intent(inout) :: this
        logical, intent(in) :: is_player
        integer :: i
        this%bodyLen = 1
        this%direction = DIRECTION_UP
        this%idleTicks = -1
        this%resetOnIdleEnd = .FALSE.
        this%is_player = is_player
        do i = 1, MAX_BODY_LEN
            this%body(i) = Point(-1,-1)
        end do
    end subroutine init_snake

    !don't actually move, just try
    function test_move_snake(this) result(collision)
        type(Snake_t), intent(in) :: this
        integer :: new_x, new_y
        integer :: collision
        integer(chtype) :: cell
        new_x = this%head%x
        new_y = this%head%y

        select case(this%direction)
        case(DIRECTION_UP)
            new_y = modulo(this%head%y - 1, fieldMaxY)
        case(DIRECTION_RIGHT)
            new_x = modulo(this%head%x + 2, fieldMaxX)
        case(DIRECTION_DOWN)
            new_y = modulo(this%head%y + 1, fieldMaxY)
        case(DIRECTION_LEFT)
            new_x = modulo(this%head%x - 2, fieldMaxX)
        end select

        cell = mvwinch(field, new_y, new_x)
        if (iand(cell, A_CHARTEXT) /= ichar(" ")&
            .and. iand(cell, A_CHARTEXT) /= ichar("*")) then
            if(iand(cell, A_COLOR) == COLOR_PAIR(7) &
                .or. iand(cell, A_COLOR) == COLOR_PAIR(8)) then
                collision = COLLISION_AI_PLAYER
            else
                collision = COLLISION_AI_AI
            end if
            return
        else
            collision = COLLISION_NONE
            return
        end if
    end function test_move_snake

    !@return a collision type
    function move_snake(this) result(collision)
        type(Snake_t), intent(inout) :: this
        integer :: collision
        integer :: i
        integer :: new_x, new_y
        integer(chtype) :: cell
        new_x = this%head%x
        new_y = this%head%y

        select case(this%direction)
        case(DIRECTION_UP)
            new_y = modulo(this%head%y - 1, fieldMaxY)
        case(DIRECTION_RIGHT)
            new_x = modulo(this%head%x + 2, fieldMaxX)
        case(DIRECTION_DOWN)
            new_y = modulo(this%head%y + 1, fieldMaxY)
        case(DIRECTION_LEFT)
            new_x = modulo(this%head%x - 2, fieldMaxX)
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
            call generate_food()
            collision = COLLISION_NONE
            return
        else if (iand(cell, A_CHARTEXT) /= ichar(" ")) then
            if(iand(cell, A_COLOR) == COLOR_PAIR(7) &
                .or. iand(cell, A_COLOR) == COLOR_PAIR(8)) then
                collision = COLLISION_AI_PLAYER
            else
                collision = COLLISION_AI_AI
            end if
            return
        else
            collision = COLLISION_NONE
            return
        end if
    end function move_snake

    subroutine display_snake(this)
        type(Snake_t), intent(in) :: this
        integer :: i
        integer(C_LONG) :: ierr
        integer :: headColor, bodyColor

        if(this%is_player) then
            headColor = CP_PLAYER_HEAD
        else
            headColor = CP_AI_HEAD
        end if
        if(this%idleTicks>-1) then
            bodyColor = CP_AI_HEAD
        else if(this%is_player) then
            bodyColor = CP_PLAYER_BODY
        else
            bodyColor = CP_AI_BODY
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
        type(Snake_t), intent(inout) :: this
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
        select case(modulo(irand(), 10))
        case (0)
            call turn_left(this)
        case (1)
            call turn_right(this)
        end select
        res = move_snake(this)
        if(res /= COLLISION_NONE) then
            this%idleTicks = 5
            this%resetOnIdleEnd = .TRUE.
            if(res == COLLISION_AI_PLAYER) then
                call eat_food(player)
            end if
        end if
    end subroutine advance_AI

    subroutine clear_snake(this)
        type(Snake_t), intent(inout) :: this
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
        type(Snake_t), intent(inout) :: this
        integer :: side
        side = modulo(irand(),4)
        select case(side)
        case(0) !top side
            this%bodyLen = modulo(irand(), MAX_AI_BODY_LEN)+1
            this%head%x = alignX(modulo(irand(), fieldMaxX))
            this%head%y = 0
        case(1) !right side
            this%bodyLen = modulo(irand(), MAX_AI_BODY_LEN)+1
            this%head%x = fieldMaxX
            this%head%y = modulo(irand(), fieldMaxY+1)
        case(2) !bottom side
            this%bodyLen = modulo(irand(), MAX_AI_BODY_LEN)+1
            this%head%x = alignX(modulo(irand(), fieldMaxX))
            this%head%y = fieldMaxY
        case(3) !left side
            this%bodyLen = modulo(irand(), MAX_AI_BODY_LEN)+1
            this%head%x = 0
            this%head%y = modulo(irand(), fieldMaxY+1)
        end select
    end subroutine respawn_AI_snake

    function alignX(x)
        integer, intent(in) :: x
        integer :: alignX
        alignX = x
        if (modulo(x, 2) == 1) then
            alignX = modulo(x + 1, fieldMaxX)
        end if
        return
    end function alignX

    subroutine set_pos_center(this)
        type(Snake_t), intent(inout) :: this
        !setup and align the snake you start with
        this%head%x = alignX(fieldMaxX/2)
        this%head%y = fieldMaxY/2
        !add body
        this%body(1)%x = this%head%x - 1
        this%body(1)%y = this%head%y
    end subroutine set_pos_center

    subroutine turn_left(this)
        type(Snake_t), intent(inout) :: this
        this%direction = modulo(this%direction - 1, 4)
    end subroutine turn_left

    subroutine turn_right(this)
        type(Snake_t), intent(inout) :: this
        this%direction = modulo(this%direction + 1, 4)
    end subroutine turn_right

    !increases snake length by 1, adds boost
    subroutine eat_food(this)
        type(Snake_t), intent(inout) :: this
        this%bodyLen = this%bodyLen + 1
        if(this%is_player .eqv. .TRUE.) then
            boostTicks = int(boostTicks + BOOST_TIME_MS/ &
                (get_sleep_time_us(this%bodyLen, .FALSE.)/1000.0))
        end if
    end subroutine eat_food

    !spawns a new food entity
    subroutine generate_food()
        integer :: ierr, x, y
        integer(C_LONG) :: ch
        ch = ichar(" ")
        do
            x = modulo(irand(), fieldMaxX)
            y = modulo(irand(), fieldMaxY)
            !align food
            x = x + modulo(x, 2)
            if (iand(mvwinch(field, y, x), A_CHARTEXT) == ch) exit
        end do
        ierr = wattron(field, COLOR_PAIR(CP_FOOD))
        ierr = mvwaddch(field, y, x, ichar("*", 8))
        ierr = wattroff(field, COLOR_PAIR(CP_FOOD))
    end subroutine generate_food

    subroutine draw_info(this, debug)
        type(Snake_t), intent(in) :: this
        logical, intent(in) :: debug
        integer(C_LONG) :: ierr
        character(len=3) :: str
        ierr = move(1, 0)
        ierr = clrtoeol()
        if (.NOT. can_change_color()) then
            !draw separator at the end of the header
            ierr = mvhline(HEADER_HEIGHT - 1, 0, 0_C_LONG, mwMaxX)
        end if
        ierr = mvprintw(0, 0, "Press q to exit"//C_NULL_CHAR)
        ierr = mvprintw(1, 0, "Length: "//C_NULL_CHAR)
        if (boostTicks > 0) then
            ierr = attron(COLOR_PAIR(CP_ALERT_TEXT))
            ierr = mvprintw(1, mwMaxX/2 - 4, "BOOSTING"//C_NULL_CHAR)
            ierr = attroff(COLOR_PAIR(CP_ALERT_TEXT))
        end if
        write(str, '(i3)') this%bodyLen
        ierr = mvprintw(1, 8, str)

        if(debug) then
            ierr = mvprintw(0, mwMaxX-5, "debug"//C_NULL_CHAR)
        end if
    end subroutine draw_info

    integer function get_sleep_time_us(len, applyBoost)
        integer, intent(in) :: len
        logical, intent(in) :: applyBoost
        integer :: sleep_time
        real :: intensity
        sleep_time = 6000000
        if (boostTicks > 0 .AND. applyBoost) then
            intensity = BOOST_INTENSITY
        else
            intensity = 1.0
        end if
        get_sleep_time_us = int(sleep_time/(len+15) * intensity)
    end function get_sleep_time_us
end module snake
