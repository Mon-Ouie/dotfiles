#!/bin/sh

setxkbmap fr bepo
xmodmap ~/.Xmodmap

# xrandr -s 1920x1080
xcompmgr &

subtle 2>log &
pid=$!

tmux new-session -d -s irc
tmux new-window -t irc:2 -n "irc" "TERM=screen-256color weechat-curses"

tmux new-session -d -s other

tmux new-window -t other:2 -n "mail" "TERM=screen-256color mutt"
tmux new-window -t other:3 -n "music" "TERM=screen-256color ncmpcpp"

urxvt -name irc_term -e zsh -c "tmux attach -t irc"     &
urxvt -name urxvt_other -e zsh -c "tmux attach -t other" &

firefox &
emacs &

wait $pid
