# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Tracktime Integration - Couple known locations
TRACKTIME_LOCATIONS="~/projects/tracktime ~/tracktime"
for tracktime in $TRACKTIME_LOCATIONS; do
    if eval [ -f ${tracktime}/completion.bash ]; then
	eval source ${tracktime}/completion.bash ${tracktime} ${HOME}/projects
	break
    fi
done

# Keep the admin commands in my PATH and ~/bin
PATH=$PATH:/sbin:/usr/sbin
PATH=$PATH:~/bin

export HISTSIZE=100000
export MPD_HOST=sajou
export MPD_PORT=6600
export EDITOR=emacsclient
export PYTHONSTARTUP=~/.pythonrc
export PAGER=less
export LESS="-I-q-s-F"

# User specific aliases and functions
xset b off 2>&-
alias vlc='vlc --zoom=2 '
alias bc='bc -lq'
alias n="screen -x normal "
alias gerp=grep

# Domain / Site specific configurations
DOMAIN=$(dnsdomainname 2>&-)
if [ -n "${DOMAIN}" -a -r ~/.bashrc.${DOMAIN} ]; then
    echo "DEBUG: sourcing ~/.bashrc.${DOMAIN}"
fi
# Special "domains"
DOMAINS_SPECIAL="home work laptop"
for d in $DOMAINS_SPECIAL; do
    if [ -r ~/.bashrc.${d} ]; then
	echo "DEBUG: sourcing ~/.bashrc.${d}"
	source ~/.bashrc.${d}
    fi
done

#if [ "$PS1" != "" -a "${STARTED_SCREEN:-x}" = x ]
#then
#  STARTED_SCREEN=1 ; export STARTED_SCREEN
#  [ -d $HOME/lib/screen-logs ] || mkdir -p $HOME/lib/screen-logs
#  sleep 1
#  screen -RR && exit 0
#  # normally, execution of this rc script ends here...
#  echo "Screen failed! continuing with normal bash startup"
#fi

[ -f ~/.proxy ] && source ~/.proxy
