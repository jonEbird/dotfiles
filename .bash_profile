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
