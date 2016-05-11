#!/bin/bash

# this is a script to install all necessary programs
# Copyright Chris Maier


function help (){
    echo "$1 [-a|--all] [-t|--tiny]"
}

while [[ $# > 1 ]]
do
    case $1 in
	-a|--all)
	    DISTRO="full"
	    shift # past argument
	    ;;
	-t|--tiny)
	    DISTRO="tiny"
	    shift # past argument
	    ;;
	*|-h|--help)
	    help
	    shift
	    ;;
    esac
done

PACKAGES = "exuberant-ctags \
	 ispell iamerican ingerman \
	 graphviz \
	 doxygen doxygen-doc \
	 vim-gtk \
	 mc \
	 curl \
	 git \
	 zsh"

if [ $DISTRO == "full" ]
then
    PACKAGES += "texlive-full \
    	     thunderbird \
	     revelation \
	     pdftk \
	     pwgen \
	     google-chrome-stable \
	     clang \
	     cmake "
fi

# Install tools
apt-get install --yes\
	$PACKAGES

add-apt-repository -y ppa:ubuntu-elisp
apt-get update
apt-get install emacs-snapshot

# fix missing packages
apt-get install -f
apt-get update
apt-get upgrade

#echo "Do forget to install Ghostery!!!"
#read

# Make a link between gnome-terminal and cmd.
ln -s /usr/bin/gnome-terminal /usr/bin/cmd
ln -fs /usr/bin/emacs-snapshot /usr/bin/emacs
