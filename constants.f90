module constants
    use ncurses
    implicit none

    !control
    integer, parameter :: MODE_SNAKE                        = 0
    integer, parameter :: MODE_SNAKE_AI                     = 1
    integer, parameter :: MODE_SCORES                       = 2

    integer, parameter :: COLLISION_NONE                    = 0
    integer, parameter :: COLLISION_AI_AI                   = 1
    integer, parameter :: COLLISION_AI_PLAYER               = 2

    integer, parameter :: DIRECTION_UP                      = 0
    integer, parameter :: DIRECTION_RIGHT                   = 1
    integer, parameter :: DIRECTION_DOWN                    = 2
    integer, parameter :: DIRECTION_LEFT                    = 3

    integer, parameter :: EXIT_NONE                         = 0
    integer, parameter :: EXIT_MANUAL                       = 1
    integer, parameter :: EXIT_GAME_OVER                    = 2
    integer, parameter :: EXIT_WIN                          = 3

    integer, parameter :: SKEY_EXIT                         = ichar("q",8)
    integer, parameter :: SKEY_LEFT                         = ichar("a",8)
    integer, parameter :: SKEY_RIGHT                        = ichar("d",8)
    integer, parameter :: SKEY_ADVANCE                      = ichar(" ",8)
    integer, parameter :: SKEY_ENTER                        = 10
    integer, parameter :: SKEY_ESCAPE                       = 27

    !ui
    integer, parameter :: HEADER_HEIGHT                     = 4
    character(len=*), parameter :: APP_NAME                 = "FORTRANSNAKE"

    !colors
    integer, parameter :: CP_AI_HEAD               = 1
    integer, parameter :: CP_AI_BODY               = 2
    integer, parameter :: CP_FOOD                  = 3
    integer, parameter :: CP_BACKGROUND            = 4
    integer, parameter :: CP_DEFAULT               = 5
    integer, parameter :: CP_ALERT_TEXT            = 6
    integer, parameter :: CP_PLAYER_HEAD           = 7
    integer, parameter :: CP_PLAYER_BODY           = 8

    !balancing
    integer, parameter :: FOOD_AMOUNT                       = 3
    integer, parameter :: BOOST_TIME_MS                     = 3000
    real, parameter :: BOOST_INTENSITY                      = 0.6
    integer, parameter :: MAX_BODY_LEN                      = 200
    integer, parameter :: MAX_AI_COUNT                      = 5
    integer, parameter :: MAX_AI_BODY_LEN                   = 15

    !files
    character(len=*), parameter :: SCORES_FILE              = "scores"
contains
end module constants
