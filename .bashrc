# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Tracktime Integration - Couple known locations
TRACKTIME_LOCATIONS="~/projects/tracktime ~/tracktime"
for tracktime in $TRACKTIME_LOCATIONS; do
    if [ -f ${tracktime}/completion.bash ]; then
	eval source ${tracktime}/completion.bash ${tracktime} ${HOME}/projects
	break
    fi
done

# Keep the admin commands in my PATH and ~/bin
PATH=$PATH:/sbin:/usr/sbin
PATH=$PATH:~/bin

# Standard PS1
PS1="[\u@\h \W]\$ "

gitps1() {
    _git_repo() {
        basename $(git remote -v | awk '/^origin.*(fetch)/{ print $2 }') | sed 's/\.git//g'
    }

    if [ -r ~/git-prompt.sh ]; then
        . ~/git-prompt.sh
        PS1='[\u@\h \W$(__git_ps1 " (%s)")]\$ '
        #PS1='[\u@\h \W ($(_git_repo):$(__git_ps1 "%s)")]\$ '
    fi
}

export HISTSIZE=100000
export MPD_HOST=sajou
export MPD_PORT=6600
export EDITOR=emacsclient
export PYTHONSTARTUP=~/.pythonrc
export PAGER=less
export LESS="-I-q-s-F-R"
export WORKON_HOME=~/venv

# User specific aliases and functions
xset b off 2>&-
alias vlc='vlc --zoom=2 '
alias bc='bc -lq'
alias n='normal-screen '
alias gerp='grep '
alias grpe='grep '
alias sudo='sudo '
alias ls='ls --color=auto -F '
alias magit='emacsclient -a emacs -e "(magit-status \"$(pwd)\")"'

# Domain / Site specific configurations
DOMAIN=$(dnsdomainname 2>&-)
if [ -d ~/.bashrc.${DOMAIN}.d ]; then
    for bashrc in ~/.bashrc.${DOMAIN}.d/*; do
	source ${bashrc}
    done
fi
MACHINE=$(hostname | sed 's/^\([^\.]*\).*$/\1/g')
if [ -d ~/.bashrc.${MACHINE}.d ]; then
    for bashrc in ~/.bashrc.${MACHINE}.d/*; do
	source ${bashrc}
    done
fi

# virtualenvwrapper
if type -P virtualenvwrapper.sh 1>/dev/null 2>&1; then
    source virtualenvwrapper.sh
fi

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
