#!/bin/bash

MUSIC_PLAYER_X=${1:-30}
MUSIC_PLAYER_Y=${2:-40}

#------------------------------

# Track where our mouse is currently
eval $(xdotool getmouselocation --shell)

# Move to the music player's pause button,
#  click it,
#  then move back
xdotool mousemove $MUSIC_PLAYER_X $MUSIC_PLAYER_Y click 1 mousemove $X $Y
