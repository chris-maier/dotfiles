#!/bin/bash

browser_packages=" google-chrome-stable"

source shellib.sh
CheckSudo

wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -

echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list

AptInstallList ${browser_packages}

CmdAvailable "google-chrome"
if [ $? -eq 0 ]; then
    # update alternatives if it is installed
    update-alternatives --install /usr/bin/x-www-browser x-www-browser $(which google-chrome) 60
fi
