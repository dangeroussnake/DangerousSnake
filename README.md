<p align="center">
  <a href="https://github.com/dangeroussnake/DangerousSnake">
    <img alt="DangerousSnake" src="https://dangeroussnake.de/res/icon/DangerousSnake144x144.png" width="144">
  </a>
</p>

<p align="center">
  Snake in <b>FORTRAN</b>
</p>

## Description
A simple snake game written in FORTRAN using gfortran and [ncurses for FORTRAN][1].

### Gameplay
<table cellspacing="0" cellpadding="0">
    <tr>
        <td style="text-align: center;">
            <img src="https://dangeroussnake.de/res/gameplay/MainMenu.png" alt="main menu" />
        </td>
        <td style="text-align: center;">
            <img src="https://dangeroussnake.de/res/gameplay/Snake.png" alt="snake" />
        </td>
    </tr>
    <tr>
        <td style="text-align: center;">
            <img src="https://dangeroussnake.de/res/gameplay/SnakeWithAI.png" alt="snake with AI" />
        </td>
        <td style="text-align: center;">
            <img src="https://dangeroussnake.de/res/gameplay/Scores.png" alt="scores" />
        </td>
    </tr>
</table>

## Dependencies
[ncurses port for Fortran 2003][1] (slightly modified)

## How to build on UNIX
- install gfortran
- install the ncurses libraries
- `make`

### Installation on Ubuntu
```
sudo apt install gfortran libncurses-dev
git clone https://github.com/dangeroussnake/DangerousSnake.git
cd DangerousSnake
make
./snake
```

## About
DangerousSnake and its [website](https://dangeroussnake.de) are developed and maintained by the [DangerousSnake Organization](https://github.com/dangeroussnake).


[1]: http://www.urbanjost.altervista.org/LIBRARY/libscreen/ncurses/pdsrc/ncurses_from_Fortran.html