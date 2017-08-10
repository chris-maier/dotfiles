#!/bin/bash
# setup mail accounts

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source shellib.sh
CheckSudo

packages="offlineimap notmuch msmtp msmtp-mta msmtp-gnome libsecret-tools python-gnomekeyring"

AptInstallList $packages
