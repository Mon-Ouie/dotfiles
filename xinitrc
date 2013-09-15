#!/bin/sh

setxkbmap fr bepo
xmodmap ~/.Xmodmap

# xrandr -s 1920x1080
# xcompmgr &

devmon -g &

# For QGtkStyle
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

subtle 2>log &
pid=$!

if [ "$(mpc status|grep playing)" = "" ]; then
    sh ~/code/dotfiles/reset_player.sh
fi

tmux new -d -s other -n "music" "TERM=screen-256color ncmpcpp"
tmux new-window -t other:2 -n "stats" "TERM=screen-256color htop"

urxvt -name irc_term -e /bin/bash -c "sh ~/code/dotfiles/weechat.sh" &
urxvt -name urxvt_other -e /bin/bash -i -c "tmux attach -t other" &

firefox &
emacs &

wait $pid
