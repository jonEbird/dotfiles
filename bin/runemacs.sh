#!/usr/bin/env bash

# This is created primarily for MacOS where I occasionally need to start
# Emacs from a terminal vs. from Spotlight.

# FYI, if you want to debug your init, add '--debug-init'

# If you'd like to profile your init, run:
# emacs -Q -l <PATH>/profile-dotemacs.el -f profile-dotemacs

usage() {
    cat <<EOF
Usage: $(basename -- $0) [--debug|--profile] [emacs-args]
       Starts emacs for you and passes along any arguments
       --debug     = gets translated to '--debug-init'
       --profile   = ends up using a profiler to test your config
EOF
}

# FIXME: Remove these annoying '#' leading recentf files:
sed -i '/^[[:space:]]*#/d' ~/.emacs.d/recentf

if [[ $(uname) == Darwin ]]; then
    EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
    # Need to add in bin directory so Emacs can find 'emacsclient'
    PATH="$PATH:$(brew --prefix emacs)/bin"
else
    EMACS=emacs
fi

args=()
case ${1:-} in
    --debug)
        args+=( --debug-init )
        shift
        ;;
    --profile)
        # Run M-x el-get-update RET profile-dotemacs RET to update
        # Can also be found here: http://www.randomsample.de/profile-dotemacs.el
        profiler=~/.emacs.d/el-get/profile-dotemacs/profile-dotemacs.el
        args+=(-Q -l "$profiler" -f profile-dotemacs)
        shift
        ;;
    --help|-h|help)
        usage
        exit 0
        ;;
esac

echo Debug: Running $EMACS "${args[@]}" "$@"
$EMACS "${args[@]}" "$@"
