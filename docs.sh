#!/bin/bash

fromBase="$HOME/Google Drive"
toBase="$HOME/Documents"
dirs=("misc" "resume" "texts")

cd "$fromBase"

for dir in ${dirs[@]}; do
    echo "$toBase/$dir"
    if [[ -h "$toBase/$dir" ]]; then
        printf "overwriting $dir, ok? [Y/N]"
        read a
        if [[ $a == "N" || $a == "n" ]]; then
            printf "...skipping $dir\n\n"
            continue
        else
            printf "...overwriting $dir.\n\n"
            rm "$toBase/$dir"
        fi
    fi
    ln -s "$fromBase/$dir" "$toBase"
done
