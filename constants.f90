module constants
    implicit none

    !control
    integer, parameter :: MODE_SNAKE                = 0
    integer, parameter :: MODE_SNAKE_AI             = 1

    integer, parameter :: COLLISION_NONE            = 0
    integer, parameter :: COLLISION_AI_AI           = 1
    integer, parameter :: COLLISION_AI_PLAYER       = 2

    integer, parameter :: DIRECTION_UP              = 0
    integer, parameter :: DIRECTION_RIGHT           = 1
    integer, parameter :: DIRECTION_DOWN            = 2
    integer, parameter :: DIRECTION_LEFT            = 3

    integer, parameter :: EXIT_NONE                 = 0
    integer, parameter :: EXIT_MANUAL               = 1
    integer, parameter :: EXIT_GAME_OVER            = 2
    integer, parameter :: EXIT_WIN                  = 3

    integer, parameter :: SKEY_EXIT                 = ichar("q",8)
    integer, parameter :: SKEY_LEFT                 = ichar("a",8)
    integer, parameter :: SKEY_RIGHT                = ichar("d",8)
    integer, parameter :: SKEY_ADVANCE              = ichar(" ",8)
    integer, parameter :: SKEY_ENTER                = 10
    integer, parameter :: SKEY_ESCAPE               = 27

    !ui
    integer, parameter :: headerHeight = 4

    !balancing
    integer, parameter :: foodAmount = 3
    integer, parameter :: boostTime_ms = 3000
    real, parameter :: boostIntensity = 0.6
    integer, parameter :: maxBody = 200
    integer, parameter :: maxAICount = 5
    integer, parameter :: aiMaxBodyLen = 15

contains
end module constants
