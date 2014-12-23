# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

# Helpful functions
epoch2date() {
    date --date="@${1:-0}" +"${2:-%c}"
}

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
