#!/bin/bash

MUSIC_PLAYER_X=${1:-24}
MUSIC_PLAYER_Y=${2:-930}
date >> /tmp/$(basename $0).log
#------------------------------

# Track where our mouse is currently
eval $(xdotool getmouselocation --shell)

# Move to the music player's pause button,
#  click it,
#  then move back
xdotool mousemove $MUSIC_PLAYER_X $MUSIC_PLAYER_Y click 1 mousemove restore # $X $Y
#xdotool mousemove $MUSIC_PLAYER_X $MUSIC_PLAYER_Y mousedown 1 sleep 0.1 mouseup 1 mousemove $X $Y

