# .bash_profile

# Un-comment for profiling
# PS4='+ $(date "+%s.%N")\011 '
# exec 3>&2 2>/tmp/bashstart.$$.log
# set -x

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# User specific environment and startup programs

# Support for org-protocol
if which gconftool-2 >/dev/null 2>&1 && [ -n "$DISPLAY" -a -z "$SSH_CLIENT" ]; then
    gconftool-2 -s /desktop/gnome/url-handlers/org-protocol/command "$(which emacsclient) %s" --type String
    gconftool-2 -s /desktop/gnome/url-handlers/org-protocol/enabled --type Boolean true
fi

# Screen / Tmux setup
if [ -f /usr/share/doc/tmux/examples/bash_completion_tmux.sh ]; then
    source /usr/share/doc/tmux/examples/bash_completion_tmux.sh
fi

# Domain / Site specific configurations
DOMAIN=$(dnsdomainname 2>&-)
if [ -d ~/.bash_profile.${DOMAIN}.d ]; then
    for bashprofile in ~/.bash_profile.${DOMAIN}.d/*; do
	source ${bashprofile}
    done
fi
MACHINE=$(hostname | sed 's/^\([^\.]*\).*$/\1/g')
if [ -d ~/.bash_profile.${MACHINE}.d ]; then
    for bashprofile in ~/.bash_profile.${MACHINE}.d/*; do
	source ${bashprofile}
    done
fi

using-brew() {
    # Check if this is a Mac machine using Homebrew
    which brew >/dev/null 2>&1 && [[ $(uname) == "Darwin" ]]
}

brew-profile-is-stale() {
    # check if the cached brew profile content needs to be updated
    # Return true (0) if we need to update the cached profile
    local boot_time_s brew_profile_s
    boot_time_s=$(date --date="$(ps -o lstart -p 1 | tail -1)" +%s)
    # BSD style 'stat' command in use here
    brew_profile_s=$(/usr/bin/stat -f %m ~/.bash_profile.brew 2>&-)
    [[ -z "$brew_profile_s" ]] && return 0
    [[ $boot_time_s -gt $brew_profile_s ]] && return 0
    return 1
}

# Let's only do the heavy brew stuff once per reboot
if using-brew; then
    if brew-profile-is-stale; then
        cat <<EOF > ~/.bash_profile.brew
# Bash Completion
if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi

# Add basic brew bin location to the top of the PATH
PATH=$(brew --prefix)/bin:\$PATH

# For the benefit of my alias in .bashrc:
export LS=gls

# Coreutils
PATH="$(brew --prefix coreutils)/libexec/gnubin:\$PATH"
export MANPATH="$(brew --prefix coreutils)/share/man:\$MANPATH"

# Python
PYBASE=$(brew --prefix python)
PATH="\$PYBASE/bin:\$PATH"
export MANPATH="\$PYBASE/share/man:\$MANPATH"
VIRTUALENVWRAPPER_PYTHON="$(command \which python)"
# if ! [[ -h "\$PYBASE/bin/python" ]]; then
#     ln -s "\$(readlink "\$PYBASE/bin/python2.7")" "\$PYBASE/bin/python"
# fi
# Miniconda
# export PATH=/Users/jonmiller/miniconda2/bin:\$PATH

# Coreutils
PATH="$(brew --prefix coreutils)/libexec/gnubin:\$PATH"
export MANPATH="$(brew --prefix coreutils)/share/man:\$MANPATH"

# Findutils
PATH="$(brew --prefix findutils)/libexec/gnubin:\$PATH"
MANPATH="$(brew --prefix findutils)/libexec/gnuman:\$MANPATH"

# Go environment setup
export GOROOT=$(brew --prefix go)/libexec
export GOPATH=\$HOME/go
[[ -d \$GOPATH ]] || mkdir \$GOPATH
export PATH=\$PATH:\$GOPATH/bin

# Sphinx-doc
PATH=\$PATH:$(brew --prefix sphinx-doc)/bin

# Other Mac specific items
alias cal='gcal'
EOF
    fi
    source ~/.bash_profile.brew
fi

# Un-comment when profiling
# set +x
# exec 2>&3 3>&-

export PATH="$HOME/.cargo/bin:$PATH"
[[ -s "$HOME/.avn/bin/avn.sh" ]] && source "$HOME/.avn/bin/avn.sh" # load avn
