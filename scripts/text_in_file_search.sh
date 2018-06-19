#! /bin/sh
#
# text_in_file_search.sh
# Copyright (C) 2018 jeremy-minton <jeremy.minton@eigentech.com>
#
# Distributed under terms of the MIT license.
#

if [ "$#" -eq 1 ]; then
    grep -rnw . -e $1 --exclude='*.pyc' --color=auto
elif [ "$#" -eq 2 ]; then
    grep -rnw $1 -e $2 --exclude='*.pyc' --color=auto
else
    grep -rnw $1 -e $2 --exclude=$3 --color=auto
fi
