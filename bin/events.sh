#!/bin/bash

PATH="/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:/sbin:/usr/sbin"
TMP=/tmp/.events$$_

EVENTS=${TMP}events

boottime() {
    local seconds_since_boot=$(awk '{ print int($1) }' /proc/uptime)
    echo $(($(date +%s) - $seconds_since_boot))
}

file_mtime() {
    echo "$(stat -c %Y $1) - File $item" >> $EVENTS
}

file_atime() {
    echo "$(stat -c %X $1) - File atime $item" >> $EVENTS
}

rpm_installtime() {
    echo "$(rpm -q --qf="%{installtime}\n" $1) - RPM $item installed" >> $EVENTS
}
declare -A querytype=( [mtime]=file_mtime [atime]=file_atime )

#--------------------------------------------------
echo "$(boottime) - System last boot" >> $EVENTS

for item in "$@"; do

    # Support different types of items
    # 1. special directive
    qtype=$(echo $item | awk -F: '{ print $1 }')
    if [ -n "${querytype[$qtype]}" ]; then

        fun=${querytype[$qtype]}
        item=$(echo $item | awk -F: '{ print $2 }')
        eval $fun $item

    # 2. files
    elif [ -f $item ]; then
        file_mtime $item

    # 3. RPM installation time
    elif rpm -q $item >/dev/null 2>&1; then
        rpm_installtime $item

    else
        echo "Warning: Do not know how to handle \"${item}\"" 1>&2

    fi
done

sort -n $EVENTS | awk '{ print strftime("%F %T", $1) " - " $0 }'
rm -f ${TMP}*
