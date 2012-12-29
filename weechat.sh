#!/usr/bin/env sh

create_irc_session () {
    tmux new -d -s irc -n "irc" "TERM=screen-256color weechat-curses"
    tmux attach -t irc
}

if [ `tmux has-session -t irc` ]; then
    tmux attach -t irc
else
    case `hostname` in
        *-laptop) # maybe use SSH
            read -p "Host of the computer running weechat? (localhost) " remote
            echo $remote

            if [ "$remote" = "" ]; then
                create_irc_session
            else
                ssh $remote -t "LC_ALL=en_EN.UTF-8 tmux attach -t irc"
            fi
            ;;

        *-desktop) create_irc_session;;
    esac
fi
