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

# option flags
OPT_TOOLS=true
OPT_DEV=false
OPT_YOCTO=false
OPT_DESKTOP=false
OPT_EMACS=false
OPT_BROWSER=false
OPT_VIM=false
OPT_NEOVIM=false
OPT_ZSH=false

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

APT_GET_OPTIONS="--yes --show-progress --install-suggests --auto-remove"
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
				OPT_DEV=true
				PACKAGES+=$DEV
				;;
			+yocto)
				OPT_YOCTO=true
				PACKAGES+=$YOCTO
				;;
			+desktop)
				OPT_DESKTOP=true
				PACKAGES+=$DESKTOP
				;;
			+emacs)
				OPT_EMACS=true
				install_emacs
				;;
			+neovim)
				OPT_NEOVIM=true
				install_neovim
				;;
			+vim)
				OPT_VIM=true
				PACKAGES+=$VIM
				;;
			+browser)
				OPT_BROWSER=true
				install_browser
				;;
			+zsh)
				OPT_ZSH=true
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
	wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
	echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list
	PACKAGES+=$BROWSER
}

function post_install_browser (){
	local BROWSER_BIN=$(which $BROWSER)
	update-alternatives --install /usr/bin/x-www-browser x-www-browser $BROWSER_BIN 60
}

function install_emacs (){
	add-apt-repository -y ppa:ubuntu-elisp
	PACKAGES+=$EMACS
}

function post_install_emacs (){
	local EMACS_BIN=$(which emacs)
	# post install emacs
	if [ -n $EMACS_BIN ]; then
		ln -fs $SCRIPT_DIR/../src/.emacs.d ~/.emacs.d
	fi
}

function install_neovim (){
	# add ppa repository
	add-apt-repository ppa:neovim-ppa/stable
	# add packages 
	PACKAGES+=$NEOVIM
}

function post_install_neovim (){
	local NEOVIM_BIN=$(which nvim)
	local EDITOR_BIN=$(which editor)

	# link the config files to ~/.config/neovim/
	ln -fs $SCRIPT_DIR/../src/.config/nvim ~/.config/nvim/

	# install as default editor
	update-alternatives --install $EDITOR_BIN editor $NEOVIM_BIN 60
}

function install_zsh (){
	PACKAGES+=$ZSH
}

function post_install_zsh ()
{
	local ZSH_BIN=$(which zsh)

	# post install zsh
	if [ -n $ZSH_BIN ]; then
		# change login shell of current user, not root
		chsh -s $ZSH_BIN $SUDO_USER
		# download and install oh-my-zsh
		sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
		ln -fs $SCRIPT_DIR/../src/.zshrc ~/.zshrc
	fi
}

function install_packages (){

	if $OPT_BROWSER; then 
		install_browser
	fi

	if $OPT_DESKTOP; then 
		PACKAGES+=$DESKTOP
	fi

	if $OPT_DEV; then 
		PACKAGES+=$DEV
	fi

	if $OPT_EMACS; then 
		install_emacs
	fi

	if $OPT_NEOVIM; then 
		install_neovim
	fi

	if $OPT_TOOLS; then 
		PACKAGES+=$TOOLS
	fi

	if $OPT_VIM; then 
		PACKAGES+=$VIM
	fi

	if $OPT_YOCTO; then 
		PACKAGES+=$YOCTO
	fi

	if $OPT_ZSH; then 
		install_zsh
	fi

	# remove duplicates
	PACKAGES=$(printf '%s\n' $PACKAGES | sort -u)
	# Install tools
	apt-get update
	apt-get install $PACKAGES $APT_GET_OPTIONS
	apt-get upgrade --yes
}

function post_install (){
	local MATE=$(which mate-terminal)
	local GNOME=$(which gnome-terminal)

	if [ -n $MATE ]; then
		ln -fs $MATE /usr/bin/cmd
	elif [ -n $GNOME ]; then
		ln -fs $GNOME /usr/bin/cmd
	else
		echo "No terminal shortcut set"
	fi

	if $OPT_BROWSER; then 
		post_install_browser
	fi

	if $OPT_EMACS; then 
		post_install_emacs
	fi

	if $OPT_NEOVIM; then 
		post_install_neovim
	fi

	if $OPT_ZSH; then 
		post_install_zsh
	fi

}

#
# Main starts here
check_sudo
parse_args $*
install_packages
post_install
