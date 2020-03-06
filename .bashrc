# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# Helpful functions
epoch2date() {
    date --date="@${1:-0}" +"${2:-%c}"
}

# TZ conversion functions
utc-to-pacific() { TZ=America/Los_Angeles date --date="TZ=\"GMT\" $*" +"%H:%M %Z"; }
pacific-to-utc() { TZ=GMT date --date="TZ=\"America/Los_Angeles\" $*" +"%H:%M %Z"; }

pathgrep() { echo $PATH | sed 's/:/ /g' | xargs ls 2>/dev/null | grep -i ${1:-.}; }

color-echo() {
    # Use like: color-echo "Hello red{devil}, are you missing the yellow{sun}?"
    declare -A colors
    colors["red"]=31; colors["green"]=32; colors["yellow"]=33; colors["blue"]=34
    echo $@ | sed -e "s/red{\([^}]*\)}/\x1b[${colors['red']}m\1\\x1b[0m/g" \
        -e "s/red{\([^}]*\)}/\x1b[${colors['red']}m\1\\x1b[0m/g" \
        -e "s/green{\([^}]*\)}/\x1b[${colors['green']}m\1\\x1b[0m/g" \
        -e "s/yellow{\([^}]*\)}/\x1b[${colors['yellow']}m\1\\x1b[0m/g" \
        -e "s/blue{\([^}]*\)}/\x1b[${colors['blue']}m\1\\x1b[0m/g"
}

blue ()   { color-echo "blue{$*}"; }
green ()  { color-echo "green{$*}"; }
red ()    { color-echo "red{$*}"; }
yellow () { color-echo "yellow{$*}"; }

# Disable the annoying message about the default shell changing to zsh
export BASH_SILENCE_DEPRECATION_WARNING=1

# Keep the admin commands in my PATH and ~/bin
PATH=$PATH:/sbin:/usr/sbin
PATH=$PATH:~/bin

gitps1() {
    _git_repo() {
        basename "$(git remote -v | awk '/^origin.*(fetch)/{ print $2 }')" | sed 's/\.git//g'
    }

    if [ -r ~/git-prompt.sh ]; then
        . ~/git-prompt.sh
        PS1='[\u@\h \W$(__git_ps1 " (%s)")]\$ '
        #PS1='[\u@\h \W ($(_git_repo):$(__git_ps1 "%s)")]\$ '
    fi
}

gpr() {
    # Create a pull request from the current feature branch
    local symbolic_rev
    symbolic_rev=$(git rev-parse --abbrev-ref HEAD)
    if [[ $symbolic_rev == HEAD ]]; then
        echo "Cannot create PR in detached HEAD mode." && return 1
    elif [[ $symbolic_rev =~ ^dev$|^master$|^release$ ]]; then
        echo "Not creating PR from '$symbolic_rev'. Please use a feature branch instead." && return 2
    fi
    git push origin "$symbolic_rev" -u
    hub compare "jonmiller:$symbolic_rev"
}

# # Moved to using Native Docker for Mac
# if docker-machine ls -q 2>/dev/null | grep -E '^default$' >/dev/null; then
#     eval "$(docker-machine env default)"
# fi

export HISTSIZE=100000
export MPD_HOST=sajou
export MPD_PORT=6600
export EDITOR=less  # emacsclient
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
export LS=gls
PATH=/opt/homebrew/opt/python/bin:$PATH
alias ls="\${LS:-ls} --color=auto -F "
alias tree='tree --charset=ascii '
alias magit='emacsclient -a emacs -e "(magit-status \"$(git rev-parse --show-toplevel)\")"'
projectile() {
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

# PS1 and related Status
if [ -n "${INSIDE_EMACS:-}" ]; then
    if [ -n "$EMACS_PS1" ]; then
        PS1="$EMACS_PS1"
    else
        PS1="\W $ "
    fi
    # export PAGER=emacspager
    export TERM=eterm-color
    export GIT_PAGER="cut -c 1-${COLUMNS-80}"  # alternative: alias git='git --no-pager '
    # Using virtualenvwrapper Emacs package which sets this
    unset VIRTUAL_ENV
    PATH=$(echo $PATH | sed 's/:/\n/g' | grep -v "^$WORKON_HOME" | tr '\n' ':' | sed 's/:$//g')
else
    # Standard PS1
    PS1="[\u@\h \W]\$ "
    export TERM="xterm-256color"
fi

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

[ -f ~/.bash_profile.brew ] && source ~/.bash_profile.brew

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
[ -f ~/.bashrc.work ] && source ~/.bashrc.work

eval "$(rbenv init - 2>&-)"
PATH=$PATH:~/repos/pe-infra/rio-toolbox/ciborg
export REQUESTS_CA_BUNDLE=/opt/homebrew/etc/openssl/cert.pem

# virtualenvwrapper
if type -P virtualenvwrapper.sh 1>/dev/null 2>&1; then
    export VIRTUALENVWRAPPER_PYTHON=/opt/homebrew/bin/python
    source virtualenvwrapper.sh
fi

# pyenv via: brew install pyenv pyenv-virtualenvwrapper
eval "$(pyenv init -)"
# export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="true"

show_virtual_env() {
    if [[ -n "$VIRTUAL_ENV" && -n "$DIRENV_DIR" ]]; then
        echo "[$(basename $VIRTUAL_ENV)]"
    fi
}
export -f show_virtual_env
PS1='$(show_virtual_env)'$PS1

# direnv via: brew install direnv
eval "$(direnv hook bash)"

export NVM_DIR="$HOME/.nvm"
. $(brew --prefix nvm)/nvm.sh
