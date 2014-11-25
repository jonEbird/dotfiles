#!/bin/bash

PATH="/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:/sbin:/usr/sbin"
TMP=/tmp/.events$$_

EVENTS=${TMP}events
declare -A querytype
declare -A querytype_help

#-Standard-Functions-------------------------------
boottime() {
    local seconds_since_boot=$(awk '{ print int($1) }' /proc/uptime)
    echo $(($(date +%s) - $seconds_since_boot))
}

rpm_installtime() {
    echo "$(rpm -q --qf="%{installtime}\n" $1) - RPM $item installed" >> $EVENTS
}

usage() {
    cat <<EOF
Usage: $(basename $0) [events [...]]

$(basename $0) helps identify a chronological order of events that occured
on your system. You can pass a variety of events, in various forms, and
they will be investigated and printed out in sorted time order. There are
two "normal" forms which do not require any prefix help and they are
interpretted as either a filename / directory or a package (RPM). Special
forms require a "prefix:" where "prefix" is a helper to indicate how to
query the information with the directive after the colon.

Here is a summary of the Events:
  - boottime - Last boot time of the machine (Always included)
  - <filename> - Modification time of file or directory
  - <package> - Installation time of <package>
  - datetime:message - Manual entry to be included. "datetime" part should
                       be parsable by date.
EOF
for key in "${!querytype[@]}"; do
    echo "  - ${key}:argument - ${querytype_help[$key]}"
done
}

#-Special-Form-Functions---------------------------
file_mtime() {
    echo "$(stat -c %Y $1) - File $item" >> $EVENTS
}
querytype[mtime]=file_mtime
querytype_help[mtime]="Query the modification time of the file argument"

file_atime() {
    echo "$(stat -c %X $1) - File atime $item" >> $EVENTS
}
querytype[atime]=file_atime
querytype_help[atime]="Query the access/read time of the file argument"

grep_messages() {
    local pat="$1" ldate=""
    # Including /dev/null ensures you get the "file:line excerpt" output
    # from grep when only /var/log/messages exists
    \ls -tr /var/log/messages* | xargs -i sudo zgrep "$pat" /dev/null {} | \
        sed -e 's|^/var/log/||g' -e 's/:/ - /1' | \
    while read line; do
        ldate=$(date --date="$(echo $line | awk '{ print $3 " " $4 " " $5 }')" +%s)
        echo "$ldate - $line" >> $EVENTS
    done
}
querytype[messages]=grep_messages
querytype_help[messages]="Grep the /var/log/messages* files for a given pattern"

grep_general() {
    local pat files file ldate
    pat=$(echo "$1" | cut -d: -f1)
    files=$(echo "$1" | cut -d: -f2-)
    # Now use echo to expand the potential globbing pattern
    files=$(echo $files)
    for file in $files; do
        # FIXME: Should ideally look for valid datetime patterns in each line
        ldate=$(stat -c %Y $file)
        zgrep "$pat" "$file" | \
            awk -v D=$ldate -v F=$file '{ print D " - " F ":" $0 }' >> $EVENTS
    done
}
querytype[grep]=grep_general
querytype_help[grep]="General Grep. argument should be \"pattern:fileglob\""

#--------------------------------------------------
if [ "$1" == "--help" -o "$1" == "-h" -o "$1" == "help" -o -z "$1" ]; then
    usage
    exit 1
fi

echo "$(boottime) - System last boot" >> $EVENTS
echo "$(date +%s) - NOW"              >> $EVENTS

for item in "$@"; do

    # Support different types of items
    # 1. special directive
    qtype=$(echo $item | awk -F: '{ print $1 }')
    if [ -n "${querytype[$qtype]}" ]; then

        fun=${querytype[$qtype]}
        item=$(echo $item | cut -d: -f2-)
        eval $fun "$item"

    # 2. files
    elif [ -f "$item" ]; then
        file_mtime $item

    # 3. RPM installation time
    elif rpm -q "$item" >/dev/null 2>&1; then
        rpm_installtime $item

    # 4. Could be a manual message to be included
    elif date --date="$qtype" >/dev/null 2>&1; then
        msg_part="$(echo $item | cut -d: -f2-)"
        echo $(date --date="$qtype" +%s) "- $msg_part" >> $EVENTS

    else
        echo "Warning: Do not know how to handle \"${item}\"" 1>&2

    fi
done

sort -n $EVENTS | awk '{ print strftime("%F %T", $1) " - " $0 }'
rm -f ${TMP}*
