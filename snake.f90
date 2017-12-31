module snake
  use ncurses
  implicit none

  type :: Point
    integer :: x,y
  end type Point
  integer :: direction
  type(Point) :: head
  integer, parameter :: maxBody=200
  type(Point), dimension(maxBody) :: body
  integer :: bodyLen
  !max x and y of the whole window
  integer :: mwMaxX, mwMaxY
  !max x and y of the subwindow used as playfield
  integer :: fieldMaxX, fieldMaxY
  integer :: headerHeight = 4
  integer :: foodAmount = 3
  type(C_PTR) :: field

contains

  subroutine init()
    integer :: i
    bodyLen=1
    direction=1
    do i=1, maxBody
      body(i)%x=-1
      body(i)%y=-1
    end do
    call getmaxyx(field,fieldMaxY,fieldMaxX)
    !align field
    fieldMaxX = fieldMaxX-MODULO(fieldMaxX,2)
    !setup the snake you start with
    head%x=fieldMaxX/2
    head%y=fieldMaxY/2
    body(1)%x=head%x-1
    body(1)%y=head%y
    !align snake
    if(MODULO(head%x,2)==1) then
      head%x=head%x+1
    end if
    do i=1, foodAmount
      call generate_food()
    end do
  end subroutine init

  subroutine move_snake(mexit)
    integer :: mexit, i
    integer :: new_x,new_y
    integer(C_LONG) :: ch, ierr
    new_x=head%x
    new_y=head%y

    select case(direction)
    case(0)
      new_y=MODULO(head%y-1, fieldMaxY)
    case(1)
      new_x=MODULO(head%x+2, fieldMaxX)
    case(2)
      new_y=MODULO(head%y+1, fieldMaxY)
    case(3)
      new_x=MODULO(head%x-2, fieldMaxX)
    end select

  !move body
    do i=bodyLen+1, 2, -1
      body(i)=body(i-1)
    end do

    body(1)=head !correct

  !move head
    head%x=new_x
    head%y=new_y

  !check collision
    ch = mvwinch(field,new_y,new_x)
    if(ch==ichar("*")) then
      if(bodyLen+2>maxBody) then
        ierr = mvwprintw(field,6,0,"You won!")
      else
        bodyLen=bodyLen+1
      end if
      call generate_food()
    else if(ch==ichar("0")) then
      mexit=2
    end if

  !render
    ierr=mvwaddch(field,head%y,head%x,ichar("X",8))
    do i=1, bodyLen
      ierr=mvwaddch(field,body(i)%y,body(i)%x,ichar("0",8))
    end do
    ierr=mvwaddch(field, body(bodyLen+1)%y, body(bodyLen+1)%x, ichar(" ",8))
  end subroutine move_snake

  subroutine turn_left()
    direction=MODULO(direction-1,4)
  end subroutine turn_left

  subroutine turn_right()
    direction=MODULO(direction+1,4)
  end subroutine turn_right

  subroutine generate_food()
    integer :: ierr, x,y
    integer(C_LONG) :: ch

    ch=ichar(" ")
    do
      x=MODULO(irand(),fieldMaxX)
      y=MODULO(irand(),fieldMaxY)
      if(MODULO(x,2)==1) then
        x=x+1
      end if
      if(mvwinch(field,y,x)==ch) exit
    end do
    ierr=mvwaddch(field,y,x,ichar("*",8))
  end subroutine generate_food

  subroutine draw_info(debug)
    logical :: debug
    integer(C_LONG) :: ierr
    integer :: i
    character*3 :: str

    !draw seperator at the end of the header
    ierr = mvhline(headerHeight-1, 0, 0_C_LONG, mwMaxX)

    ierr=mvprintw(0,0, "Press q to exit"//C_NULL_CHAR)
    ierr=mvprintw(1,0, "Length: "//C_NULL_CHAR)
    write(str,'(i3)') bodyLen
    ierr=mvprintw(1,8,str)
  end subroutine draw_info

end module snake

integer function get_sleep_time(len)
  integer :: sleep_time = 4000000
  get_sleep_time=INT(sleep_time/(len+10))
end function get_sleep_time

program main
  use ncurses
  use snake
  implicit none

  interface
     subroutine usleep(useconds) bind(C)
      use iso_c_binding
      implicit none
      integer(c_int32_t), value :: useconds
     end subroutine
  end interface
  logical :: debug = .FALSE.
  integer :: get_sleep_time
  integer :: ierr, ikey
  !0 is no exit, 1 is intentional exit, 2 is game over
  integer :: mexit=0
  integer(C_LONG) :: ch
  integer,parameter :: seed = 129085
  stdscr=initscr()
  ierr=start_color()
  ierr=cbreak()
  ierr=noecho()
  ierr=nodelay(stdscr, logical(.TRUE.,1))
  ierr=curs_set(0)
  call srand(seed)
  ierr=init_pair(1_C_SHORT, COLOR_RED, COLOR_BLACK)
  ierr=init_pair(2_C_SHORT, COLOR_GREEN, COLOR_BLACK)

  call getmaxyx(stdscr, mwMaxY, mwMaxX)
  field = newwin(mwMaxY-headerHeight, mwMaxX, headerHeight, 0);
  call init()
  do
    if (mexit/=0) exit
    ikey=getch()
    select case(ikey)
    case(ichar("a",8))
      call turn_left()
    case(ichar("d",8))
      call turn_right()
    case(ichar("q",8))
      mexit=1
    case (ichar(" ",8))
      call move_snake(mexit)
    end select
    call draw_info(debug)
    call move_snake(mexit)
    ierr=refresh()
    ierr=wrefresh(field)
    call usleep(get_sleep_time(bodyLen))
  end do
  if(mexit==2) then
    ierr=attron(COLOR_PAIR(1))
    ierr=mvprintw(1, mwMaxX/2-5, "Game over!")
    ierr=attroff(COLOR_PAIR(1))
    ierr=nodelay(stdscr,logical(.FALSE.,1))
    ikey=getch()
  end if
  ierr=delwin(field)
  ierr=endwin()
end program main
