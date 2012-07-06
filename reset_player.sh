#!/usr/bin/env sh

mpd ~/.mpd.conf
mpc clear
mpc load general
mpc random on

case `hostname` in
    *-laptop)  mpc pause;;
    *-desktop) mpc play;;
esac
