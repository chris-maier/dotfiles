#!/bin/bash

set -x

# this is a script to install all necessary programs
# Copyright Chris Maier

# Package definitions
DEV=" clang cmake doxygen doxygen-docs graphviz mc wget curl git exuberant-ctags ksh g++"
YOCTO=" gawk wget git-core diffstat unzip texinfo gcc-multilib build-essentials chrpath socat libsdl1.2-dev xterm"
DESKTOP=" thunderbird revelation pdftk pwgen google-chrome-stable texlive-full"
EMACS=" emacs-snapshot"
VIM=" vim-gtk"
ZSH=" zsh"

#
# Function definitions
#

function usage (){
    echo "$1 [+dev] [+yocto] [+desktop] [+emacs] [+vim] [+zsh]"
    exit 1
}

function parse_args (){
    while [[ $# > 0 ]]
    do
	case $1 in
	    +dev)
		PACKAGES+=$DEV
		;;
	    +yocto)
		PACKAGES+=$YOCTO
		;;
	    +desktop)
		PACKAGES+=$DESKTOP
		;;
	    +emacs)
		install_emacs
		;;
	    +vim)
		PACKAGES+=$VIM
		;;
	    +zsh)
		install_zsh
		;;
	    *)
		usage
		;;
	esac
	shift
    done
}

function check_sudo (){
    # Make sure only root can run our script
    if [ "$(id -u)" != "0" ]; then
	echo "This script must be run as root"
	exit 1
    fi
}

function install_emacs (){
    add-apt-repository -y ppa:ubuntu-elisp
    PACKAGES+=$EMACS
}

function install_zsh (){
    # download and install oh-my-zsh
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
    PACKAGES+=$ZSH
}

function install_packages (){
    # remove duplicates
    PACKAGES=$(printf '%s\n' $PACKAGES | sort -u)
# Install tools
    # apt-get update
    # apt-get install --yes\ $PACKAGES
    # apt-get upgrade --yes
}

function install_shortcuts (){
    local MATE=$(which mate-terminal)
    local GNOME=$(which gnome-terminal)

    if [ -n $MATE ]; then
	ln -s $MATE /usr/bin/cmd
    elif [ -n $GNOME ]; then
	ln -s $GNOME /usr/bin/cmd
    else
	echo "No terminal shortcut set"
    fi
}

#
# Main starts here
check_sudo
parse_args $*
install_packages
install_shortcuts
