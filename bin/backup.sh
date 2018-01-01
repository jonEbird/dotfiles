#!/bin/bash

# Script: Backup Workstation
# Author: jsmiller@qti.qualcomm.com
# Date:   2014-04-28

PATH="/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:/sbin:/usr/sbin"
TMP="/tmp/.BackupWorkstation$$_"

CONFIG_LOC=~/.config/jonEbird/backup_rsync_target
VERBOSE=""

#-Functions----------------------------------------
cleanup() { rm -rf ${TMP}*; }

prep-rsync-filelist() {
    touch ${TMP}left-for-dead
    local infile=$1
    local outfile=$2
    > $outfile # start fresh
    for globpat in $(sed -r '/^(#|$)/d' $infile); do
        # Doing this will expand file globs
        for file in $(echo $globpat); do
            if [ -e "$file" ]; then
                echo "$file" >> $outfile
            else
                echo "$file" >> ${TMP}left-for-dead
            fi
        done
    done
}

usage() {
    [ -n "$1" ] && echo "$@" 1>&2
    echo "Usage: $(basename $0) [-v] [-n] [rsync-target]" 1>&2
    echo "       -v = verbose, -n = dry-run"
    echo "       rsync-target is taken from $CONFIG_LOC when not specified" 1>&2
}

# --------------------------------------------------
# Need to reside in HOME for a couple of reasons
# 1. Generating the list of files from HOME vantage point
# 2. The --files-from rsync option uses a relative path and we're using "."
cd $HOME

# -Personal-/-Home-directory-items------------------
cat <<EOF > ${TMP}home_file_list
# While it should be committed, back it up anyway
$(git ls-files)
.git*

# Personal stuff
personal
Photos
Pictures
Videos
Webcam

# Work stuff
org
projects
published
repos
snippets
trylogs
Documents

# Email / Desktop stuff
.msmtp.log
.mailrc
iMacros
Maildir
Downloads
bin
.conkyrc
.erc
.msmtp.log
.msmtprc
.offlineimap
.offlineimaprc
.mu
.netrc.gpg
.notmuch-config
.aspell.en*
.hunspell_en_US
.purple
.Qualcomm-sig.txt
.Gmail-sig.txt
.screenrc*
.tmux.conf
.ssh
.subversion
.thunderbird

# Emacs stuff
.emacs.d/eshell
.emacs.d/abbrev_defs
.emacs.d/smex-items
.emacs.d/projectile*
.emacs.d/el-get-user
.emacs.d/my_configs

# Misc but important
.virtualenvs
venv
.backups
.bash*
.zsh*
.config
.mozilla
.lesshst
.keybase
.keybase-installer
.gnupg
.ido.last.*
.pip
.pypirc
.pythonrc
.rpmmacros
.stackato
.vim*
.vnc
.m2
EOF

# -Personal-/-Home-directory-items------------------
cat <<EOF > ${TMP}system_file_list

# Network Manager files collected
/etc/sysconfig/network-scripts/ifcfg-*
/etc/sysconfig/network-scripts/keys*

# Printers
/etc/printcap
/etc/cups/*.conf
/etc/cups/ppd/*.ppd

EOF

#-Main---------------------------------------------
trap cleanup SIGINT SIGQUIT SIGTERM SIGSEGV

RSYNC_OPTIONS="-ar --delete"

while getopts :vn OPT; do
    case $OPT in
        v|+v)
            RSYNC_OPTIONS="$RSYNC_OPTIONS -v"
            VERBOSE="yes"
            ;;
        n|+n)
            RSYNC_OPTIONS="$RSYNC_OPTIONS -n"
            ;;
        *)
            usage
            exit 2
    esac
done
shift `expr $OPTIND - 1`
OPTIND=1

DESTINATION="$1"
if [ -z "$DESTINATION" ]; then
    if [ -r $CONFIG_LOC ]; then
        DESTINATION=$(cat $CONFIG_LOC)
    else
        usage "No rsync-target nor saved $CONFIG_LOC"
        exit 3
    fi
else
    if [ ! -e $CONFIG_LOC ]; then
        mkdir -p "$(dirname $CONFIG_LOC)" >/dev/null 2>&1
        echo "$1" > $CONFIG_LOC
    fi
fi

# Make the list more acceptable to rsync
prep-rsync-filelist ${TMP}system_file_list ${TMP}rsync-list

echo -n "Performing system backup first..."
START=$SECONDS
[ -n "$VERBOSE" ] && echo "Rsync system via: sudo rsync $RSYNC_OPTIONS --files-from=${TMP}rsync-list / ${DESTINATION}system/"
sudo rsync $RSYNC_OPTIONS --files-from=${TMP}rsync-list / "${DESTINATION}system/"
RC=$?
if [ $RC -eq 0 ]; then
    echo "completed in $((SECONDS-START))s"
else
    echo "Problem with the backup"
fi

# Make the list more acceptable to rsync
prep-rsync-filelist ${TMP}home_file_list ${TMP}rsync-list

echo -n "Performing home backup..."
START=$SECONDS
[ -n "$VERBOSE" ] && echo "Rsync home via: rsync $RSYNC_OPTIONS --files-from=${TMP}rsync-list . ${DESTINATION}home/"
rsync $RSYNC_OPTIONS --files-from=${TMP}rsync-list . "${DESTINATION}home/"
RC=$?
if [ $RC -eq 0 ]; then
    echo "completed in $((SECONDS-START))s"
else
    echo "Problem with the backup"
fi


if [ -s ${TMP}left-for-dead ]; then
    echo "Warrning: The following files were not backed up because they don't exist:"
    sed 's/.*/  &/g' ${TMP}left-for-dead
fi

# Cleanup
# cleanup
exit $RC
