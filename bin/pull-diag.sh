#!/bin/bash

# Script: Pull Diagnostics
#   Used to collect data from machines when something has gone wrong. Used
# after the fact to collect postmortem logs from the machine(s). Data
# collection can be tedious, so using this script was born. Also support
# ways to easily insert links to the files for org-mode notes.
# Author: jsmiller@qti.qualcomm.com
# Date:   2014-04-28

PATH="/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:/sbin:/usr/sbin"
TMP="/tmp/.PullDiag$$_"
REL_DIR=$(dirname $0)

#-Functions----------------------------------------
cleanup() { rm -rf ${TMP}*; }

org-store-file () {
    local f fp
    for f in $*; do
        cd "$(dirname $f)"
        fp="file:$(pwd | sed "s|^${HOME}|~|g")/$(basename $f)"
        emacsclient -e "(add-to-list 'org-stored-links '(\"$fp\" \"$(basename $f)\"))" >/dev/null
        cd ~-
    done
}

usage() {
    cat <<EOF
Usage: $(basename $0) [options] host [nexthost|..]
       -p|--project  - Specify which project to associate collected data with
       -b|--basedir  - Base directory where data is centrally kept
       -d|--datetime - A specific instance identifier typically in YYYMMDD_HHMM form
       -h|--help     - Show this help
       -l|--list     - (TODO) list collected data

  Idea is to ssh into the client machine and collect important data and
  store it in a <basedir>/<project>/<machine>/YYYYMMDD_HHMM structure ala:
    documents/                 (default $BASEDIR)
    └── project                (default $PROJECT)
        ├── machine1
        │   ├── 20140407_1235  (currently $DATETIME)
        │   └── 20140408_1417
        ├── machine2
        │   ├── 20140421_1036
        │   └── 20140428_1542
        └── machine3
            ├── 20140407_1320
            └── 20140408_1543
EOF
}

#-Main---------------------------------------------
trap cleanup SIGINT SIGQUIT SIGTERM SIGSEGV

# Defaults
BASEDIR="~/Documents/clientlogs"
PROJECT="common"
DATETIME=$(date +%Y%m%d_%H%M)

# Local config overrides?
CONFIG_LOC=~/.config/jonEbird/pull-diag
[ -f $CONFIG_LOC ] && source $CONFIG_LOC

# How do we do what we do?
GET_LOGFILE_CMD=""

#-Process-options----------------------------------
TEMP=$(getopt -o p:b:d:vlh -l project:,basedir:,datetime:,list,help -n "$(basename -- $0)" -- "$@")
if [ $? != 0 ] ; then echo "died in getopt"; usage; exit 1; fi

eval set -- "$TEMP"

while true ; do
    case "$1" in
        -b|--basedir)
            shift; BASEDIR="$1"; shift
            ;;
        -p|--project)
            shift; PROJECT="$1"; shift
            ;;
        -l|--list)
            shift; echo "TODO: Method unimplemented"
            exit 0
            ;;
        -d|--datetime)
            shift; DATETIME=$(date --date="$1" +%Y%m%d_%H%M); shift
            ;;
        -h|--help)
            usage; exit 0
            ;;
	--) shift ; break ;;
	*) echo "Problem parsing option \"$1\"" ; exit 2 ;;
    esac
done


for host in "$@"; do
    eval LOGDIR="${BASEDIR}/${PROJECT}/${host}/${DATETIME}"
    [ ! -d $LOGDIR ] && mkdir -p "$LOGDIR"

    TGT="${LOGDIR}/${host}_messages"
    N=0
    while [ -f $TGT ]; do
        N=$((N+1))
        TGT="${LOGDIR}/${host}_messages.${N}"
    done

    echo "Going to pull messages from \"$host\" -> $TGT"
    scp root@${host}:/var/log/messages $TGT
    org-store-file $TGT
done

# Cleanup
cleanup
exit 0
