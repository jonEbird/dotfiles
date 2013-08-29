#!/bin/bash

MAILDIR="~/Maildir"

#------------------------------
EXTRA_FILTER=""
SUMMARY="no"

while getopts :gGs OPT; do
    case $OPT in
        g|+g)
            EXTRA_FILTER="$EXTRA_FILTER AND NOT list"
            ;;
        G|+G)
            EXTRA_FILTER="$EXTRA_FILTER AND list"
            ;;
        s|+S)
            SUMMARY="yes"
            ;;
        *)
            echo "usage: `basename $0` [+-g} [--] ARGS..."
            exit 2
    esac
done
shift `expr $OPTIND - 1`
OPTIND=1

# Primarilly used for my conky status
case $1 in
    *[qQ]ualcomm*)
        MBOX_QUERY="m:/Qualcomm/INBOX"
        ;;
    *[gG]mail*)
        MBOX_QUERY="m:/Gmail/INBOX"
        ;;
    *)
        if [ -n "$1" ]; then
            if [ -d "${MAILDIR}/${1}" ]; then
                MBOX_QUERY="m:$1"
            else
                echo "Unknown Maildir \"${1}\". Ignoring" 1>&2
                MBOX_QUERY=""
            fi
        fi
esac

if [ $SUMMARY == "yes" ]; then
    # Ignore any EXTRA_FILTER options
    MSG=""
    non_list=$(mu find --nocolor flag:unread $MBOX_QUERY AND NOT list 2>/dev/null | wc -l)
    MSG="${non_list}"
    list=$(mu find --nocolor flag:unread $MBOX_QUERY AND list 2>/dev/null | wc -l)
    if [ $list -gt 0 ]; then
        MSG="$MSG ($list in groups)"
    fi
    # done
    echo $MSG
else
    #echo DEBUG: mu find --nocolor flag:unread $MBOX_QUERY $EXTRA_FILTER
    mu find --nocolor flag:unread $MBOX_QUERY $EXTRA_FILTER 2>/dev/null | wc -l
fi
