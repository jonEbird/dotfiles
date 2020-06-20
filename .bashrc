# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

source ~/.mkpath  # dynamically updated via ~/bin/mkpath
source ~/.bash.functions
source ~/.bash.work

# Bash completion from brew ($BREW_PREFIX comes from .mkpath)
[[ -f ${BREW_PREFIX}/etc/bash_completion ]] && . ${BREW_PREFIX}/etc/bash_completion

export HISTSIZE=100000
export EDITOR=less  # emacsclient
export PYTHONSTARTUP=~/.pythonrc
export PAGER=less
export LANG=en_US.UTF-8
export LESS="-I-q-s-F-R"

# User specific aliases and functions
# xset b off 2>&-
alias vlc='vlc --zoom=2 '
alias bc='bc -lq'
alias grep='grep --color=auto '
alias gerp='grep '
alias grpe='grep '
alias tree='tree --charset=ascii '
alias magit='emacsclient -a emacs -e "(magit-status \"$(git rev-parse --show-toplevel)\")"'
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
else
    # Standard PS1
    PS1="[\u@\h \W]\$ "
    export TERM="xterm-256color"
fi

# f-u flow control
stty -ixon >/dev/null 2>&1
stty -ixoff >/dev/null 2>&1

show_virtual_env() {
    if [[ -n "$VIRTUAL_ENV" && -n "$DIRENV_DIR" ]]; then
        local venv_name=${VIRTUAL_ENV%/.venv} # strip .venv
        venv_name=${venv_name##*/} # basename
        venv_name=${venv_name#*-} # strip leading "category-" if present
        echo "[$venv_name]"
    fi
}
export -f show_virtual_env
PS1='$(show_virtual_env)'$PS1

# Disable the annoying message about the default shell changing to zsh
export BASH_SILENCE_DEPRECATION_WARNING=1

export HOMEBREW_DEVELOPER=1
export HOMEBREW_RUBY_PATH=/usr/bin/ruby

# export NVM_DIR="$HOME/.nvm"
# . $(brew --prefix nvm)/nvm.sh
# load avn
[[ -s "$HOME/.avn/bin/avn.sh" ]] && source "$HOME/.avn/bin/avn.sh"

# pyenv for Python and rbenv for Ruby runtimes
eval "$(pyenv init -)"
# eval "$(pyenv virtualenv-init -)"

eval "$(rbenv init - 2>&-)"

# direnv via: brew install direnv
eval "$(direnv hook bash)"
