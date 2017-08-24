#!/bin/bash

verbose=0

while getopts "v" opt; do
    case $opt in
        v)
            verbose=1
            ;;
    esac
done


find . -path ./.git -prune -o -type f -print | while read x; do
    x=${x#./*}
    if [[ -e "${HOME}/$x" ]]; then
        if [[ $verbose -eq 1 ]]; then
            diff -u "${HOME}/$x" "$x"
        else
            cmp -s "${HOME}/$x" "$x"
            [[ $? -eq 1 ]] && echo -e "\x1b[31;01mdiffer\x1b[0m" "${HOME}/$x" "$x"
        fi
    fi
done
