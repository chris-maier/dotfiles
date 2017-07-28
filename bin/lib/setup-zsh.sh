#!/bin/bash

zsh_packages="zsh zsh-common zsh-doc"

source shellib.sh
CheckSudo

# check binaries before doing anything
CmdAvailable "wget"
if [ $? -ne 0 ]; then
    exit 1
fi

# check zsh binary and install if it doesn"t "
CmdAvailable "zsh"
if [ $? -ne 0 ]; then
    # install zsh via apt-get
    AptInstallList $zsh_packages
fi

# change default shell only if it is not set yet
if [ -z $(getent passwd $SUDO_USER | grep -o zsh) ]; then
    sudo -u $SUDO_USER chsh -s /usr/bin/zsh
else
    EchoErr "login shell already changed."
fi

# download and install oh-my-zsh if it is not installed
if [ ! -d "/home/$SUDO_USER/.oh-my-zsh" ]; then
    # download and install oh-my-zsh
    sudo -u $SUDO_USER sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
else
    EchoErr "oh-my-zsh is already installed."
fi

# link .zshrc
sudo -u $SUDO_USER ln -fs ../../src/.zshrc ~/.zshrc
