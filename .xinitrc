gnome-screensaver &
#xscreensaver -no-splash &
exec awesome >> ~/.cache/awesome/stdout 2>> ~/.cache/awesome/stderr
#xinit gnome-session
