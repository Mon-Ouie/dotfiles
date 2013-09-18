#!/usr/bin/env sh

mkdir -p backup

backup_file() {
    if [[ -e $2 ]]; then
        cp -P $2 backup
        rm $2
    fi

    ln -s $PWD/$1 $2
}

backup_dir() {
    if [[ -e $2 ]]; then
        cp -r $2 backup
        rm -rf $2
    fi

    ln -s $PWD/$1 $2
}

mkdir -p ~/.zsh/
mkdir -p ~/.config/
mkdir -p ~/.ncmpcpp/
mkdir -p ~/.local/share/applications/

backup_file irbrc.rb      ~/.irbrc
backup_file vimrc         ~/.vimrc
backup_file zshrc.sh      ~/.zshrc
backup_file emacs.el      ~/.emacs
backup_file pryrc.rb      ~/.pryrc
backup_file rtorrent.conf ~/.rtorrent.rc
backup_file Xdefaults     ~/.Xdefaults
backup_file gemrc         ~/.gemrc
backup_file gitconfig     ~/.gitconfig
backup_file gtkrc         ~/.gtkrc-2.0
backup_file inputrc       ~/.inputrc
backup_file muttrc        ~/.muttrc
backup_file ncmpcpp.conf  ~/.ncmpcpp/config
backup_file procmailrc    ~/.procmailrc
backup_file rvmrc         ~/.rvmrc
backup_file vimperatorrc  ~/.vimperatorrc
backup_file XCompose      ~/.XCompose
backup_file Xmodmap       ~/.Xmodmap
backup_file xinitrc       ~/.xinitrc
backup_file tmux.conf     ~/.tmux.conf

backup_file applications/defaults.list \
    ~/.local/share/applications/defaults.list
for i in applications/*.desktop; do
    backup_file $i ~/.local/share/$i
done

backup_dir zsh-syntax-highlighting ~/.zsh/zsh-syntax-highlighting
backup_dir emacs.d                 ~/.emacs.d
backup_dir subtle                  ~/.config/subtle

echo "Files you may still need to install:"
echo "  — ~/.offlineimaprc"
echo "  — ~/code/dotfiles/github.gitconfig"
echo "  — ~/mpd/"
echo "  — ~/code/ [includes some shell scripts]"
echo "  — ~/doc/"
echo "  — ~/mail/"
echo "  — ~/picture/"
echo "  — ~/.msmtprc"
echo "  — ~/.weechat"
echo
echo "You may also need to set up:"
echo "  — SSH"
echo "  — RVM"
