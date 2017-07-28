#!/bin/bash
# Setup Spacemacs

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
spacemacs_packages=" emacs25"

source shellib.sh
CheckSudo

# install spacemacs
$(CmdAvailable "git")
if [ $?  -eq 0 ]; then
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
else
    exit 1
fi

# add ppa for emacs25
$(CmdAvailable "add-apt-repository")
if [ $? -eq 0 ]; then
    add-apt-repository ppa:ubuntu-elisp/ppa
else
    exit 1
fi

# install emacs25
AptInstallList $spacemacs_packages

# link .spacemacs.d to ~/.spacemacs.d
sudo -u $SUDO_USER ln -fs ${script_dir}/../../src/.spacemacs.d/ ~/.spacemacs.d

# update-alternatives
