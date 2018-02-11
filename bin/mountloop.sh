#!/bin/bash
# set -x

# $1 image to mount
# $2 mount point

partfile=$1
mountpoint=$2

# Debug log
verbose=""

function EchoErr () {
    printf "%s\n" "$*" >&2
}

# set verbose variable to activate Debug Log
function EchoDebug () {
    if [ "${verbose:=x}" != "x" ]; then
        printf "%s\n" "$*" >&2
    fi
}

function CheckSudo () {
    if [ $(id -u) -ne 0 ]; then
        EchoErr "You need more juice to execute that script."
        exit 1
    fi
    EchoDebug "sudo successful."
}

function CmdAvailable () {
    local retval=0
    if [ -z $(command -v $1) ]; then
        EchoErr $1 ": required but it's not installed."
        retval=1
    else
        EchoDebug "$1: installed."
    fi
    return $retval
}

# __Main__
CheckSudo
CmdAvailable "partx"

# check if image exits
if [ ! -f "$partfile" ] ; then
    EchoErr "Could not find image."
    exit 1
fi

# check mount point???
# if [ -z ]

partmap=$(partx -bg $partfile)
if [ "${partmap:=x}" == "x" ]; then
    EchoErr "partx returned null string."
fi

while IFS= read -r line ; do
    echo $line;
done <<< "${partmap}"
