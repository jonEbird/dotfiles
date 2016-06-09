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

if which brew >/dev/null 2>&1 && [[ $(uname) == "Darwin" ]]; then

    # Bash Completion
    if [ -f $(brew --prefix)/etc/bash_completion ]; then
        . $(brew --prefix)/etc/bash_completion
    fi

    # Coreutils
    PATH="$(brew --prefix coreutils)/libexec/gnubin:$PATH"
    export MANPATH="$(brew --prefix coreutils)/share/man:$MANPATH"

    # Findutils
    PATH="$(brew --prefix findutils)/libexec/gnubin:$PATH"
    MANPATH="$(brew --prefix findutils)/libexec/gnuman:$MANPATH"

    # Go environment setup
    export GOROOT=$(brew --prefix go)/libexec
    export GOPATH=$HOME/go
    [[ -d $GOPATH ]] || mkdir $GOPATH
    export PATH=$PATH:$GOPATH/bin

    # Other Mac specific items
    alias cal='gcal'
fi

# PS1 and related Status
if ps -o comm -p $PPID 2>/dev/null | grep -E '[Ee]macs$' >/dev/null; then
    if [ -n "$EMACS_PS1" ]; then
        PS1="$EMACS_PS1"
    else
        PS1="\W $ "
    fi
    export PAGER=emacspager
    export TERM=eterm-color
else
    # Standard PS1
    PS1="[\u@\h \W]\$ "
    export TERM="xterm-256color"
fi

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

if docker-machine ls -q 2>/dev/null | grep -E '^default$' >/dev/null; then
    eval "$(docker-machine env default)"
fi

export HISTSIZE=100000
export MPD_HOST=sajou
export MPD_PORT=6600
export EDITOR=emacsclient
export PYTHONSTARTUP=~/.pythonrc
export PAGER=less
export LANG=en_US.UTF-8
export LESS="-I-q-s-F-R"
export WORKON_HOME=~/venv

# User specific aliases and functions
# xset b off 2>&-
alias vlc='vlc --zoom=2 '
alias bc='bc -lq'
alias n='normal-tmux '
alias grep='grep --color=auto '
alias gerp='grep '
alias grpe='grep '
alias sudo='sudo '
alias ag='ag --color-match 4\;37 '
alias ls='ls --color=auto -F '
alias magit='emacsclient -a emacs -e "(magit-status \"$(git rev-parse --show-toplevel)\")"'
function projectile() {
    local project_dir="${1:-$(pwd)}"
    emacsclient -a emacs -e "(projectile-add-known-project \"${project_dir%%/}/\")"
}
org-store-file () {
    local f fp
    for f in "$@"; do
        fp=$(cd "$(dirname "$f")"; pwd -P)
        fp="file+emacs:~${fp#${HOME}}/$(basename "$f")"
        emacsclient -e "(add-to-list 'org-stored-links '(\"$fp\" \"$(basename "$f")\"))"
    done
}
alias ppjson="python -m json.tool"

# f-u flow control
stty -ixon >/dev/null 2>&1
stty -ixoff >/dev/null 2>&1

# Domain / Site specific configurations
DOMAIN=$(dnsdomainname 2>&-)
if [ -d ~/.bashrc.${DOMAIN}.d ]; then
    for bashrc in ~/.bashrc.${DOMAIN}.d/*; do
	source ${bashrc}
    done
fi
MACHINE=$(uname -n | sed 's/^\([^\.]*\).*$/\1/g')
if [ -d ~/.bashrc.${MACHINE}.d ]; then
    for bashrc in ~/.bashrc.${MACHINE}.d/*; do
	source ${bashrc}
    done
fi

# virtualenvwrapper
if type -P virtualenvwrapper.sh 1>/dev/null 2>&1; then
    source virtualenvwrapper.sh
fi

# Solarized shell themes
[ -f ~/gnome-terminal-colors-solarized/set_light.sh ] && alias light=~/gnome-terminal-colors-solarized/set_light.sh
[ -f ~/gnome-terminal-colors-solarized/set_dark.sh ]  && alias dark=~/gnome-terminal-colors-solarized/set_dark.sh

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
[ -f ~/.bash_profile.workstation  ] && source ~/.bash_profile.workstation
[ -f ~/.bash_profile.work ] && source ~/.bash_profile.work
