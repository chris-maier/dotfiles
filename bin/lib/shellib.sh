#!/bin/bash

function EchoErr () {
    printf "%s\n" "$*" >&2
}

function EchoDebug () {
    if [ -n ${verbose+x} ]; then
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

### Package Manager
# check if apt-get is installed
function AptInstalled () {
    local retval=$(CmdAvailable "apt-get")
    if [ $retval -gt 0 ]; then
        EchoErr "Aptitude is not installed."
    fi
    return $retval
}

# upgrade all packages with apt-get
function AptUpgradeAll () {
    if [ AptInstalled -eq 0 ]; then
        apt-get update
        apt-get upgrade --yes --quiet && return 0 || return 1
    fi
}

# install a list of packages with apt-get
function AptInstallList () {
    if [ AptInstalled -eq 0 ]; then
        apt-get update
        apt-get install "$*" --yes --quiet && return 0 || return 1
    fi
}
