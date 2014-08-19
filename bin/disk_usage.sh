#!/bin/bash

debug() {
    echo "DEBUG: $@"
}

smart_du() {
    local dir=$1; local sig_file_pnt=${2:-30}; local sig_whole=${3:-70}; local cur_pnt=${4:-100}
    # sig_file_pnt: what percent does a file/dir have to constitute for us to investigate further?
    # sig_whole: what total percent do the collective sig_file_pnt files have to be to _not_ report dir itself?
    if [ ! -d "$dir" ]; then
	echo "Invalid directory"
	return 1
    fi
    cd $dir

    whole_tamale=$(du -skx . | awk '{ print $1 }')
    #du -sk * | sort -n | awk -vwt=$whole_tamale '{ print $0 "\t" $1 * 100 / wt "%" }'

    local founddirt="no"
    local sigsum=0

    local sigdirs=""
    #du -sk * | while read sliver_of_tamale file; do
    for file in *; do
	# dont cross filesystem boundaries
	local cur_mnt="$(df -k . | awk '{ mnt=$NF } END { print mnt }')"
	local file_mnt=$(df -k "$file" | awk '{ mnt=$NF } END { print mnt }')
	[ "${cur_mnt}" != "${file_mnt}" ] && continue
	#[ "$(df -k . | awk '{ mnt=$NF } END { print mnt }')" != "$(df -k $file | awk '{ mnt=$NF } END { print mnt }')" ] && continue

	sliver_of_tamale=$(du -skx "$file" | awk '{ print $1 }')
	pnt=$((${sliver_of_tamale}00/${whole_tamale}))
	#debug "$(pwd)/${file} ${pnt}%"
	if [ $pnt -ge $sig_file_pnt ]; then
	    founddirt="yes"
	    if [ -d "$file" ]; then
		sigdirs="$sigdirs $file"
	    elif [ -f "$file" ]; then
		echo "$(pwd)/${file}	# $(($cur_pnt * $pnt / 100))%"
		sigsum=$(($sigsum + $sliver_of_tamale))
	    fi
	fi
    done
    # now to recursively query the _significant_ subdirs
    for subdir in $sigdirs; do
	pnt=$(($(du -skx "$subdir" | awk '{ print $1 }')00/${whole_tamale}))
	smart_du $subdir $sig_file_pnt $sig_whole $(($cur_pnt * $pnt / 100)) $cur_fs 
    done

    # reporting and returning time.
    sigsumpnt=$((${sigsum}00/${whole_tamale}))
    if [ $sigsumpnt -le $sig_whole -a $sigsum -ne 0 ]; then
	echo "$(pwd)/	# <-- look for *MORE* files here."
    fi
    if [ "$founddirt" == "no" ]; then
	# if no dirt was found in any subdirs or a single file here,
	# then this dir must have a lot of little dirt which adds up.
	echo "$(pwd)/	# <-- composed of many files."
    fi

    cd .. # its all relative, baby.
    return 0
}


#--------------------------------------------------

if [ "$(basename $0)" == "disk_usage.sh" ]; then
    smart_du ${1:-$HOME}
fi
