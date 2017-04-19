#!/bin/bash

# this is a script to install all necessary programs
# Copyright Chris Maier

# Package definitions
TOOLS=" mc wget curl git git-core unzip pwgen exuberant-ctags silversearcher-ag xsel autojump"
DEV=" clang cmake doxygen doxygen-docs graphviz mc exuberant-ctags ksh g++ subversion"
YOCTO=" gawk git-core diffstat unzip texinfo gcc-multilib build-essential chrpath socat libsdl1.2-dev xterm"
DESKTOP=" revelation pdftk texlive-full ffmpeg"
EMACS=" emacs-snapshot"
BROWSER=" google-chrome-stable"
VIM=" vim-gtk"
NEOVIM=" neovim python-dev python-pip python3-dev python3-pip python-setuptools python3-setuptools"

TRUECRYPT=" truecrypt"
VIRTUALBOX=" virtualbox virtualbox-qt"
MUTT=" mutt msmtp msmtp-mta urlview m3w libsecret-tools"

# option flags
OPT_TOOLS=true
OPT_DEV=false
OPT_YOCTO=false
OPT_DESKTOP=false
OPT_EMACS=false
OPT_BROWSER=false
OPT_VIM=false
OPT_NEOVIM=false

OPT_TRUECRPYT=false
OPT_VIRTUALBOX=false
OPT_MUTT=false
OPT_PRINTER=false

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SCRIPT_USER=$SUDO_USER

APT_GET_OPTIONS="--yes --show-progress --auto-remove"
#APT_GET_OPTIONS="-s"

#
# Logging
#
LOGFILE=$(date +"%Y-%m-%d").log
# this function prints to STDERR
echoerr() { printf "%s\n" "$*" >&2; }
echodebug()
{
	if [ -n ${VERBOSE+x} ]; then
		printf "%s\n" "$*" >&2
	fi
}

#
# Function definitions
#

function usage (){
	echoerr "$1 [+dev] [+truecrypt] [+yocto] [+desktop] [+emacs] [+vim] [+virtualbox] [+neovim] [+broswer] [+zsh] [-v|--verbose]"
	exit 1
}

function parse_args (){
	while [[ $# > 0 ]]
	do
		case $1 in
			+dev)
				OPT_DEV=true
				;;
			+yocto)
				OPT_YOCTO=true
				;;
			+desktop)
				OPT_DESKTOP=true
				OPT_TRUECRYPT=true
				OPT_VIRTUALBOX=true
				OPT_BROWSER=true
				OPT_ZSH=true
				OPT_PRINTER=true
				OPT_MUTT=true
				;;
			+emacs)
				OPT_EMACS=true
				;;
			+neovim)
				OPT_NEOVIM=true
				;;
			+vim)
				OPT_VIM=true
				;;
			+browser)
				OPT_BROWSER=true
				;;
			+zsh)
				OPT_ZSH=true
				;;
			+truecrypt)
				OPT_TRUECRYPT=true
				;;
			+virtualbox)
				OPT_VIRTUALBOX=true
				;;
			-v|--verbose)
				VERBOSE=true
				;;
			*)
				usage
				;;
		esac
		shift
	done
}

function check_sudo ()
{
	# Make sure only root can run our script
	if [ "$(id -u)" != "0" ]; then
		echoerr "This script must be run as root"
		exit 1
	fi
	echoerr "sudo successful"
}

function install_browser ()
{
	echoerr "Installing Google chrome ..."

	sudo -u $SCRIPT_USER wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -
	echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google.list
	PACKAGES+=$BROWSER
}

function post_install_browser ()
{
	echoerr "Configuring Google Chrome ..."

	local BROWSER_BIN=$(which $BROWSER)
	echodebug "Browser bin: $BROWSER_BIN"

	update-alternatives --install /usr/bin/x-www-browser x-www-browser $BROWSER_BIN 60
}

function install_printer ()
{
	echoerr "Install Brother MFC-L2700DW"
	sh -c '$SCRIPT_DIR/linux-brprinter-installer-2.1.1-1 MFC-L2700DW'
}

function install_emacs ()
{
	echoerr "Installing Emacs ..."

	add-apt-repository -y ppa:ubuntu-elisp
	PACKAGES+=$EMACS
}

function post_install_emacs (){
	echoerr "Configuring Emacs ..."

	local EMACS_BIN=$(which emacs)
	echodebug "Emacs bin: $EMACS_BIN"

	# post install emacs
	if [ -n $EMACS_BIN ]; then
		sudo -u $SCRIPT_USER ln -fs $SCRIPT_DIR/../src/.emacs.d ~/.emacs.d
	fi
}

function install_neovim (){
	echoerr "Installing Neovim ..."

	add-apt-repository -y ppa:neovim-ppa/stable
	PACKAGES+=$NEOVIM
}

function post_install_neovim (){
	echoerr "Configuring Neovim ..."

	local NEOVIM_BIN=$(which nvim)
	local EDITOR_BIN=$(which editor)

	echodebug "Neovim bin: $NEOVIM_BIN"
	echodebug "Editor bin: $EDITOR_BIN"

	sudo -u $SCRIPT_USER pip install --upgrade pip
	sudo -u $SCRIPT_USER pip3 install --upgrade pip

 	sudo -u $SCRIPT_USER pip install --user neovim
 	sudo -u $SCRIPT_USER pip3 install --user neovim

	# link the config files to ~/.config/neovim/
	sudo -u $SCRIPT_USER ln -fs $SCRIPT_DIR/../src/.config/nvim ~/.config/

	# install as default editor
	update-alternatives --install $EDITOR_BIN editor $NEOVIM_BIN 60
}

function install_truecrypt (){
	echoerr "Installing Truecrypt ..."

	add-apt-repository -y ppa:stefansundin/truecrypt
	PACKAGES+=$TRUECRYPT
}

function install_virtualbox ()
{
	echoerr "Installing Virtualbox ..."

	sudo -u $SCRIPT_USER wget -q -O - http://download.virtualbox.org/virtualbox/debian/oracle_vbox_2016.asc | apt-key add -
	sh -c 'echo "deb http://download.virtualbox.org/virtualbox/debian yakkety non-free contrib" > /etc/apt/sources.list.d/virtualbox.org.list'
	PACKAGES+=$VIRTUALBOX
}

function install_packages ()
{
	if $OPT_BROWSER; then
		PACKAGES+=$BROWSER
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

	if $OPT_TRUECRYPT; then
		install_truecrypt
	fi

	if $OPT_VIRTUALBOX; then
		install_virtualbox
	fi

	if $OPT_PRINTER; then
		install_printer
	fi

	# remove duplicates
	PACKAGES=$(printf '%s\n' $PACKAGES | sort -u)

	# Install tools
	apt-get update
	apt-get install $PACKAGES $APT_GET_OPTIONS
	apt-get upgrade --yes
}

function post_install ()
{
	local MATE=$(which mate-terminal)
	local GNOME=$(which gnome-terminal)

	echodebug "Mate: $MATE"
	echodebug "GNOME: $GNOME"

	if [ -n $MATE ]; then
		ln -fs $MATE /usr/bin/cmd
	elif [ -n $GNOME ]; then
		ln -fs $GNOME /usr/bin/cmd
	else
		echoerr "No terminal shortcut set"
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

	# link the Midnight commander config files
	sudo -u $SCRIPT_USER ln -fs $SCRIPT_DIR/../src/.config/mc ~/.config/

	# link git config
	sudo -u $SCRIPT_USER ln -fs $SCRIPT_DIR/../src/.config/git ~/.config/

	# link the xmodmap file
	sudo -u $SCRIPT_USER ln -fs $SCRIPT_DIR/../src/.Xmodmap ~/.Xmodmap
}

#
# Main starts here
check_sudo
parse_args $*
install_packages
post_install
