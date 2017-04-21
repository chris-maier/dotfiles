#!/bin/bash
set -x
packages=""
declare -a pre_install
declare -a post_install
declare -a result_install

# option flags
opt_browser=false
opt_desktop=false
opt_dev=false
opt_git=false
opt_mutt=false
opt_neovim=false
opt_printer=false
opt_tools=false
opt_truecrypt=false
opt_virtualbox=false
opt_yocto=false
opt_zsh=false

desktop_packages=" revelation pdftk texlive-full ffmpeg"
dev_packages=" clang cmake doxygen doxygen-docs graphviz mc exuberant-ctags ksh g++ subversion"
mutt_packages=" mutt msmtp msmtp-mta urlview m3w libsecret-tools"
tools_packages=" mc wget curl unzip pwgen exuberant-ctags silversearcher-ag xsel autojump"
yocto_packages=" gawk diffstat unzip texinfo gcc-multilib build-essential chrpath socat libsdl1.2-dev xterm"

script_dir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

apt_get_options="--yes --show-progress --auto-remove"
#apt_get_options+=" -s"

#
# Logging
#
logfile=$(date +"%Y-%m-%d").log
# this function prints to STDERR
echoerr() { printf "%s\n" "$*" >&2; }
echodebug()
{
	if [ -n ${verbose+x} ]; then
		printf "%s\n" "$*" >&2
	fi
}

# parse command line arguments
function parse_args (){
	while [[ $# > 0 ]]
	do
		case $1 in
			+dev)
				opt_dev=true
				;;
			+yocto)
				opt_git=true
				opt_yocto=true
				;;
			+desktop)
				opt_browser=true
				opt_desktop=true
				opt_git=true
				opt_mutt=true
				opt_printer=true
				opt_truecrypt=true
				opt_virtualbox=true
				opt_zsh=true
				;;
			+neovim)
				opt_neovim=true
				;;
			+browser)
				opt_browser=true
				;;
			+zsh)
				opt_zsh=true
				;;
			+truecrypt)
				opt_truecrypt=true
				;;
			+virtualbox)
				opt_virtualbox=true
				;;
			-v|--verbose)
				verbose=true
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

function check_options () {
    if $opt_desktop; then
	    packages+=$desktop_packages
    fi

    if $opt_dev; then
	    packages+=$dev_packages
    fi

    if $opt_tools; then
	    packages+=$tools_packages
    fi

    if $opt_yocto; then
	    packages+=$yocto_packages
    fi

    if $opt_mutt; then 
	    packages+=$mutt_packages
    fi
}

function package_manager (){
    # remove duplicates
    packages=$(printf '%s\n' $packages | sort -u)

    # Install all packages 
    apt-get update
    apt-get install $packages $apt_get_options
    apt-get upgrade $apt_get_options
}

function terminal_shortcut (){
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
}

function link_config (){
    # link the Midnight commander config files
    sudo -u $SUDO_USER ln -fs $script_dir/../src/.config/mc ~/.config/

    # link the xmodmap file
    sudo -u $SUDO_USER ln -fs $script_dir/../src/.Xmodmap ~/.Xmodmap
}

# Main
check_sudo
parse_args $*

source zsh.sh
source git.sh
source browser.sh
source neovim.sh
# truecrypt virtualbox printer 
source misc.sh

# execute the pre_install functions
for f in ${pre_install[@]}; do 
	$f
done

check_options
package_manager

# execute the post_install functions
for f in ${post_install[@]}; do 
	$f
done

# execute the result_install functions
for f in ${result_install[@]}; do 
	$f $verbose
done



