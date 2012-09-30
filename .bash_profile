# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

# Start a screen session
if screen -ls | egrep -q '[0-9]+\.normal'; then
    echo "Normal session already started."
else
    echo "Starting screen session \"normal\" in background"
    screen -dmS normal
    screen -dmr normal -X screen /usr/bin/synergys -a :6700 -d DEBUG -f
fi
