#!/bin/bash

# A convenience wrapper around wget, curl, etc.

url="$1"
if [ "$(which wget >& /dev/null; echo $?)" -eq 0 ]; then
    wget "$url"
elif [ "$(which curl >& /dev/null; echo $?)" -eq 0 ]; then
    curl -O "$url"
elif [ "$(which hg >& /dev/null; echo $?)" -eq 0 ]; then   
    # FIXME: Mac OS X readlink doesn't recognize the -f parameter.
    repo_dir="$(dirname "$(dirname "$(dirname "$(readlink -f "$BASH_SOURCE")")")")"
    hgrc_file="$repo_dir/.hg/hgrc"
    echo "You don't appear to have wget or curl installed."
    echo "Attempting Mercurial downloader extension..."
    if [[ -z $(grep 'download =' "$hgrc_file") ]]; then
        cat >> "$hgrc_file" <<EOF
[extensions]
download = $repo_dir/build/scripts/hg-downloader.py
EOF
    fi
    hg download "$url"
else
    echo "No downloader (i.e. wget or curl or hg) available."
    return 1
fi
