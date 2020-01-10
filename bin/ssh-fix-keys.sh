#!/usr/bin/env bash

set -eu

# Started from tips from IS&T ticket
# Sometimes need to physically reset the Yubikey too

blue() { echo -e "\x1B[0;34m${*}\x1B[0m"; }
red()  { echo -e "\x1B[0;31m${*}\x1B[0m"; }

usage() {
    [[ -n $1 ]] && red "$*"
    cat <<EOF
Usage: $(basename -- $0) [-f|--full] [-l|--list] [-h|--help]
Options:
   -f/--full   Removes all keys and re-adds them all (yubikey and personal)
   -l/--list   List out current ssh key status
   -h/--help   Show this help message

Normal operation is to remove just your Yubikey and then re-add it.
Use the -f/--full option if you wish to re-insert personal key as well.
EOF
}

run() {
    local msg=$1
    shift
    blue ${msg:-$1:}
    "$@"
}

info() {
    run "" ykstat
    run "Current keys within ssh agent:" ssh-add -l
}

reinsert() {
    # ykstat exits with 10 when no key is detected
    blue "Try removing your Yubikey"
    while :; do
        ykstat >/dev/null 2>&1 && true
        [[ $? -eq 10 ]] && break
        sleep 0.5
    done
    blue "Good. Now re-insert the Yubikey"
    while :; do
        ykstat >/dev/null 2>&1 && true
        [[ $? -ne 10 ]] && break
        sleep 0.5
    done
}

fix() {
    run "Removing any keys" ssh-add -D
    if ! run "Adding Yubikey to ssh-agent" ykadd; then
        if ! run "Perhaps you typed the PIN wrong" ykadd; then
            reinsert
            run "Trying again to add Yubikey to ssh-agent" ykadd
        fi
    fi
    run "Also adding personal ssh key" ssh-add
    info
}

readd_yubikey() {
    # Just remove and re-add only the Yubikey
    run "Removing the yubikey from ssh-agent" ykdel
    if ! run "Adding Yubikey to ssh-agent" ykadd; then
        if ! run "Perhaps you typed the PIN wrong" ykadd; then
            reinsert
            run "Trying again to add Yubikey to ssh-agent" ykadd
        fi
    fi
    info
}

case "${1:-}" in
    -l|--list)
        info
        exit 0
        ;;
    -h|--help)
        usage
        exit 0
        ;;
    -f|--full)
        fix
        exit 0
        ;;
    -*)
        usage "Unknown option \"$1\""
        exit 1
esac

readd_yubikey
