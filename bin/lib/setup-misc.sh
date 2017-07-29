#!/bin/bash
# install truecrypt virtualbox and the printer

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source shellib.sh
CheckSudo

packages="revelation truecrypt virtualbox virtualbox_qt"

# add truecrypt repository
add-apt-repository -y ppa:stefansundin/truecrypt

wget -q -O - http://download.virtualbox.org/virtualbox/debian/oracle_vbox_2016.asc | apt-key add -
echo "deb http://download.virtualbox.org/virtualbox/debian yakkety non-free contrib" >> /etc/apt/sources.list.d/virtualbox.org.list

AptInstallList $packages

# execute 3rd party printer script
${script_dir}/linux-brprinter-installer-2.1.1-1 MFC-L2700DW

# link the keyboard layout file
sudo -u $SUDO_USER ln -fs $(readlink -f $script_dir/../../src/.Xmodmap) ~/.Xmodmap

# install Terminal shortcut
local MATE=$(which mate-terminal)
local GNOME=$(which gnome-terminal)
local XFCE4=$(which xfce4-terminal)

if [ -n $MATE ]; then
	ln -fs $MATE /usr/bin/cmd
elif [ -n $GNOME ]; then
	ln -fs $GNOME /usr/bin/cmd
elif [ -n $XFCE4 ]; then
	ln -fs $XFCE4 /usr/bin/cmd
else
	EchoErr "No terminal shortcut set"
fi
