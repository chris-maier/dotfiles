#!/bin/bash

# this is a script to install all necessary programs
# Copyright Chris Maier

# Package definitions
TOOLS=" mc wget curl git git-core unzip pwgen exuberant-ctags" # always present
DEV=" clang cmake doxygen doxygen-docs graphviz mc wget curl git exuberant-ctags ksh g++ subversion"
YOCTO=" gawk wget git-core diffstat unzip texinfo gcc-multilib build-essential chrpath socat libsdl1.2-dev xterm"
DESKTOP=" thunderbird revelation pdftk pwgen google-chrome-stable texlive-full"
EMACS=" emacs-snapshot"
BROWSER=" google-chrome-stable"
VIM=" vim-gtk"
NEOVIM=" neovim python-dev python-pip python3-dev python3-pip"
ZSH=" zsh"

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

#
# Function definitions
#

function usage (){
    echo "$1 [+dev] [+yocto] [+desktop] [+emacs] [+vim] [+neovim] [+broswer] [+zsh]"
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
	    +neovim)
		install_neovim
		;;
	    +vim)
		PACKAGES+=$VIM
		;;
	    +browser)
	    	install_browser
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

function install_browser (){
    update-alternatives --install /usr/bin/x-www-browser x-www-browser /usr/bin/google-chrome 60

    wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
    echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list
    PACKAGES+=$BROWSER
}

function install_emacs (){
    add-apt-repository -y ppa:ubuntu-elisp
    PACKAGES+=$EMACS
}

function install_zsh (){
    PACKAGES+=$ZSH
}

function install_packages (){
    # remove duplicates
    PACKAGES=$(printf '%s\n' $PACKAGES | sort -u)
    # Install tools
    apt-get update
    apt-get install $PACKAGES --yes
    apt-get upgrade --yes
}

function install_neovim (){
    # add ppa repository
    add-apt-repository ppa:neovim-ppa/stable
    # install as default editor
    update-alternatives --install /usr/bin/editor editor /usr/bin/nvim 60
    # add packages 
    PACKAGES+=$NEOVIM
}

function post_install (){
    local MATE=$(which mate-terminal)
    local GNOME=$(which gnome-terminal)

    local ZSH_BIN=$(which zsh)
    local EMACS_BIN=$(which emacs)

    if [ -n $MATE ]; then
	ln -fs $MATE /usr/bin/cmd
    elif [ -n $GNOME ]; then
	ln -fs $GNOME /usr/bin/cmd
    else
	echo "No terminal shortcut set"
    fi

    # post install zsh
    if [ -n $ZSH_BIN ]; then
	# change login shell of current user, not root
	chsh -s $ZSH_BIN $(logname)
	# download and install oh-my-zsh
	sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
	ln -fs $SCRIPT_DIR/../src/.zshrc ~/.zshrc
    fi

    # post install emacs
    if [ -n $EMACS_BIN ]; then
	ln -fs $SCRIPT_DIR/../src/.emacs.d ~/.emacs.d
    fi
}

#
# Main starts here
check_sudo
parse_args $*
install_packages
post_install
