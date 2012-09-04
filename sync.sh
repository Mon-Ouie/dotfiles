#!/usr/bin/env sh

# Synchronizes some documents, allowing me to:
#   1. Not have to remember RSync's syntax
#   2. Have an example of how to use RSync handy

if [ $# != 1 ]; then
    echo "Usage: $0 remote-host"
    exit 1
fi

remote=$1

sync_dir () {
    from="$remote:$1/"
    to="$HOME/$1/"

    rsync -avurP "$from" "$to" || exit 1
    rsync -avurP "$to"   "$from" || exit 1
}

sync_dir "mpd"
sync_dir "picture"
sync_dir "doc"
