#!/bin/bash
FONT_NAME="SourceCodePro"
URL="https://github.com/adobe-fonts/source-code-pro/archive/2.030R-ro/1.050R-it.zip"

mkdir /tmp/adodefont
cd /tmp/adodefont
wget ${URL} -O ${FONT_NAME}
unzip -o -j ${FONT_NAME}
mkdir -p ~/.fonts
cp *.otf ~/.fonts
fc-cache -f -v
