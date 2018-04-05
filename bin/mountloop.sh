#!/bin/bash
# set -x

# $1 image to mount
# $2 partition number
# $3 mount point

image=$1
part=$2
mountpoint=$3

# constants
sectorsize=512

# Debug log
verbose="verbose"

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

# $1: number of arguments
# $2: number of expected arguments
function ParseArgs () {
	if [ "$1" -ne "$2" ]; then
		EchoErr "Illegal number of arguments."
		exit 1
	fi
}

# __Main__
CheckSudo
CmdAvailable "partx"
ParseArgs $# 3

# check if image exits
if [ ! -f "$image" ] ; then
    EchoErr "Could not find image."
    exit 1
fi

# check mount point???
# if [ -z ]

partmap=$(partx -bg $image -n $part)
if [ "${partmap:=x}" == "x" ]; then
    EchoErr "partx returned null string."
	exit 1
else
	# truncate offset and partition size from partition table
	offset=$(awk '{print $2}' <<< $partmap)
	size=$(awk '{print $5}' <<< $partmap)

	offset=$(expr $offset \* $sectorsize)
fi

mkdir -p $mountpoint
mount -o loop,offset=$offset,sizelimit=$size -o defaults $image $mountpoint
## output partition table line by line
# while IFS= read -r line ; do
#     echo $line;
# done <<< "${partmap}"

## User interaction
# mount part number 1 on default
# mntnum=1
# read -p "Mount partition number [$mntnum]: " input
# mntnum="${input:-$mntnum}"
# EchoDebug $mntnum
