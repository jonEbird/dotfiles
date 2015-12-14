#!/bin/bash

# Need to fixup PATH since I tend to install 'mu' via an el-get recipe
if [ -d ~/.emacs.d/el-get/mu4e/mu -a -x ~/.emacs.d/el-get/mu4e/mu/mu ]; then
    PATH=~/.emacs.d/el-get/mu4e/mu:$PATH
fi

verbose() {
    if [ -n "$VERBOSE" ]; then
        echo "debug: $@"
    fi
}

shell-count() {
    local mode="sum"
    if [ "$1" == "--breakout" ]; then
        mode="breakout"
        shift
    elif [ "$1" == "--breakdown" ]; then
        mode="breakdown"
        shift
    fi
    local MB="$1" summary=""
    if [ $mode == "sum" ]; then
        verbose "Executing: find "$MB" -name new -type d -print0 | xargs -0 ls | wc -l"
        find "$MB" -name new -type d -print0 | xargs -0 ls | wc -l
    elif [ $mode == "breakdown" ]; then
        verbose "Checking ML count via: grep -E -il '^List-Id: ' ${MB}/*/new/* | wc -l"
        list_cnt=$(grep -E -il '^List-Id: ' ${MB}/*/new/* | wc -l)
        verbose "Checking non ML count: grep -E -iL '^List-Id: ' ${MB}/*/new/* | wc -l"
        nonlist_cnt=$(grep -E -iL '^List-Id: ' ${MB}/*/new/* | wc -l)
        echo "$nonlist_cnt ($list_cnt)"
    elif [ $mode == "breakout" ]; then
        find "$MB" -name new -type d | \
            while read subdir; do
                cnt=$(ls "${subdir}" 2>&- | wc -l)
                if [ $cnt -gt 0 ]; then
                    echo " $(basename "$(dirname "$subdir")") $cnt"
                fi
            done
    fi
}

usage() {
    cat <<EOF
Usage: $(basename $0) [options]
   -m,--maildir=MAILDIR   Will pass "m:" + MAILDIR to "mu find" as filter
   -l,--non-list          Only show mail that is NOT part of a list
   -L,--lists             Only show mail that is part of a list
   -x,--extra=FILTER      Include the extra FILTER to "mu find"
   --list-breakdown       Provide a one-line summary of which mail is or
                          not part of a list
   --mailbox-counts       Show just a count of the mail along with the
                          maildir name
   -v,--verbose           Show verbose information such as what is being
                          passed to "mu find"
   Examples:
      $(basename $0) -m /Qualcomm/INBOX
      $(basename $0) -m '/Qualcomm/*' --mailbox-counts
      $(basename $0) -m /Gmail/INBOX --list-breakdown
EOF
}

# Defaults
EXTRA_FILTER=""
MODE="simple-count"
MAILDIR="/"
VERBOSE=""
# LISTS_ORed=$(emacsclient -e '(concat "(" my-mailing-lists-filter ")")')

# Process options
TEMP=$(getopt -o hvlLm:x: -l help,verbose,non-list,lists,list-breakdown,mailbox-counts,maildir:,extra: -n "$(basename -- $0)" -- "$@")
if [ $? != 0 ] ; then echo "died in getopt"; usage; exit 1; fi

eval set -- "$TEMP"

while true ; do
    case "$1" in
        -l|--non-list)
            EXTRA_FILTER="$EXTRA_FILTER AND NOT flag:list"; shift
            ;;
        -L|--lists)
            EXTRA_FILTER="$EXTRA_FILTER AND flag:list"; shift
            ;;
        -x|--extra)
            shift; EXTRA_FILTER="$EXTRA_FILTER $1"; shift
            ;;
        --list-breakdown)
            MODE="list-breakdown"; shift
            ;;
        --mailbox-counts)
            MODE="mailbox-counts"; shift
            ;;
        -m|--maildir)
            shift; MAILDIR="$1"; shift
            ;;
        -v|--verbose)
            VERBOSE="yes"; shift
            ;;
        -h|--help)
            usage; exit 0
            ;;
	--) shift ; break ;;
	*) echo "Problem parsing option \"$1\"" ; exit 2 ;;
    esac
done

# Primarilly used for my conky status
case $MODE in
    simple-count)
        # verbose "Simple count: mu find flag:unread m:$MAILDIR $EXTRA_FILTER"
        # mu find --nocolor flag:unread m:$MAILDIR $EXTRA_FILTER 2>/dev/null | wc -l
        shell-count $MAILDIR
        ;;
    list-breakdown)
        # # Ignore any EXTRA_FILTER options
        # MSG=""
        # verbose "Not in list: mu find flag:unread m:$MAILDIR AND NOT flag:list"
        # non_list=$(mu find --nocolor flag:unread m:$MAILDIR AND NOT flag:list 2>/dev/null | wc -l)
        # MSG="${non_list}"
        # verbose "IN list: mu find flag:unread and flag:list and m:$MAILDIR "
        # list=$(mu find --nocolor flag:unread AND flag:list and m:$MAILDIR  2>/dev/null | wc -l)
        # if [ $list -gt 0 ]; then
        #     MSG="$MSG ($list in groups)"
        # fi
        # echo $MSG
        shell-count --breakdown $MAILDIR
        ;;
    mailbox-counts)
        # verbose "Mailbox count: mu find --nocolor flag:unread m:$MAILDIR $EXTRA_FILTER -f "m" -s m | uniq -c"
        # mu find --nocolor flag:unread m:$MAILDIR $EXTRA_FILTER -f "m" -s m 2>&- | \
        #     awk '{ M[$0]++ } END { for (m in M) printf("%4s %s\n", M[m], m); }'
        shell-count --breakout $MAILDIR
        ;;
esac
