# .bash_profile

# Un-comment for profiling
# PS4='+ $(date "+%s.%N")\011 '
# exec 3>&2 2>/tmp/bashstart.$$.log
# set -x

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# ~/bin/mkpath
[[ -f ~/.mkpath ]] && source ~/.mkpath

# Support for org-protocol
if command -v gconftool-2 >/dev/null 2>&1 && [ -n "${DISPLAY:-}" ] && [ -z "${SSH_CLIENT:-}" ]; then
    gconftool-2 -s /desktop/gnome/url-handlers/org-protocol/command "$(command -v emacsclient) %s" --type String
    gconftool-2 -s /desktop/gnome/url-handlers/org-protocol/enabled --type Boolean true
fi

# Screen / Tmux setup
if [ -f /usr/share/doc/tmux/examples/bash_completion_tmux.sh ]; then
    source /usr/share/doc/tmux/examples/bash_completion_tmux.sh
fi

# Un-comment when profiling
# set +x
# exec 2>&3 3>&-

export PATH="$HOME/.cargo/bin:$PATH"
