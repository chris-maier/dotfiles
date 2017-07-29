#!/bin/bash

set -x
script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
git_packages="git git-core libgnome-keyring-dev pkg-config libc-dev"

source shellib.sh

CheckSudo

# install packages
AptInstallList $git_packages

# have to compile the libgnome-keyring
make_path="/usr/share/doc/git/contrib/credential/gnome-keyring/"
make -C $make_path

# link the config directory
if [ -d ~/.config/git ]; then
    rm -rf ~/.config/git
fi
sudo -u $SUDO_USER ln -fs $(readlink -f ${script_dir}/../../src/.config/git) ~/.config/
