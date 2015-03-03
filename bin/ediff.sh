#!/bin/bash

# Credit to http://ulf.zeitform.de/en/documents/git-ediff.html

PATH="/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:/sbin:/usr/sbin"

# test args
if [ ! ${#} -ge 2 ]; then
    echo 1>&2 "Usage: ${0} LOCAL REMOTE [MERGED BASE]"
    echo 1>&2 "       (LOCAL, REMOTE, MERGED, BASE can be provided by \`git mergetool'.)"
    exit 1
fi

# tools
_EMACSCLIENT=/bin/emacsclient
_BASENAME=/bin/basename
_CP=/bin/cp
_MKTEMP=/bin/mktemp

# args
_LOCAL=${1}
_REMOTE=${2}
if [ ${3} ] ; then
    _MERGED=${3}
else
    _MERGED=${_REMOTE}
fi

if [ ${4} -a -r ${4} ] ; then
    _BASE=${4}
    _EDIFF=ediff-merge-files-with-ancestor
    _EVAL="${_EDIFF} \"${_LOCAL}\" \"${_REMOTE}\" \"${_BASE}\" nil \"${_MERGED}\""
elif [ ${_REMOTE} = ${_MERGED} ] ; then
    _EDIFF=ediff
    _EVAL="${_EDIFF} \"${_LOCAL}\" \"${_REMOTE}\""
else
    _EDIFF=ediff-merge-files
    _EVAL="${_EDIFF} \"${_LOCAL}\" \"${_REMOTE}\" nil \"${_MERGED}\""
fi

# console vs. X
if [ -z "${DISPLAY}" ]; then
    _EMACSCLIENTOPTS="-t"
else
    _EMACSCLIENTOPTS="-c"
fi

# run emacsclient
echo ${_EMACSCLIENT} ${_EMACSCLIENTOPTS} -a "" -e "(${_EVAL})" >> /tmp/ediff.debug
${_EMACSCLIENT} ${_EMACSCLIENTOPTS} -a "" -e "(${_EVAL})" 2>&1

# check modified file
if [ ! $(egrep -c '^(<<<<<<<|=======|>>>>>>>|####### Ancestor)' ${_MERGED}) = 0 ]; then
    _MERGEDSAVE=$(${_MKTEMP} --tmpdir $(${_BASENAME} ${_MERGED}).XXXXXXXXXX)
    ${_CP} ${_MERGED} ${_MERGEDSAVE}
    echo 1>&2 "Oops! Conflict markers detected in $_MERGED."
    echo 1>&2 "Saved your changes to ${_MERGEDSAVE}"
    echo 1>&2 "Exiting with code 1."
    exit 1
fi

exit 0
