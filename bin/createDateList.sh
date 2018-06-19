#!/bin/bash
#
# This script creates files for each day of the year.
# File name pattern: prefix_YYYY_MM_DD.txt
# set -x


function main ()
{
    local prefix=$1
    local timemachine=$(date '+%F' -d 2016-01-01)

    for i in $(seq -w 0 365)
    do
        datenow=$(date '+%m%d' -d "$timemachine+$i days")
        echo $datenow >> ${prefix}_${datenow}.txt
    done
}

main $@
