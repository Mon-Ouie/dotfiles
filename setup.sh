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

backup_file irbrc.rb      ~/.irbrc
backup_file vimrc         ~/.vimrc
backup_file zshrc.sh      ~/.zshrc
backup_file emacs.el      ~/.emacs
backup_file pryrc.rb      ~/.pryrc
backup_file rtorrent.conf ~/.rtorrent.rc
backup_file Xdefaults     ~/.Xdefaults
backup_file gemrc         ~/.gemrc

backup_dir zsh-syntax-highlighting ~/.zsh/zsh-syntax-highlighting
backup_dir emacs.d                 ~/.emacs.d

echo "Files you may still need to install:"
echo "  — ~/.getmail/"
echo "  — ~/code/dotfiles/github.gitconfig"
