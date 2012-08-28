export HISTFILE=~/.zsh_history
export HISTSIZE=50000
export SAVEHIST=50000

export CLICOLOR=true
export LSCOLORS=exfxcxdxbxegedabagacad

setopt autopushd pushdminus pushdsilent pushdtohome
setopt autocd
setopt cdablevars
setopt ignoreeof
setopt interactivecomments
setopt nobanghist
setopt noclobber
setopt HIST_REDUCE_BLANKS
setopt HIST_IGNORE_SPACE
setopt SH_WORD_SPLIT
setopt nohup
setopt correctall

autoload colors
colors

export EDITOR="emacsclient -t"
export BROWSER=firefox

if [[ -s /usr/local/bin/brew ]]; then
    export PATH="/usr/local/bin:/usr/local/sbin:$PATH"
else
    export PATH="$PATH:/usr/local/bin:/usr/local/sbin"
fi

export PATH="$PATH:$HOME/.gem/ruby/1.9.1/bin" # using system ruby if it's 1.9
export PATH="$PATH:/usr/bin/vendor_perl"

export CFLAGS="-I/usr/local/include"
export LDFLAGS="-L/usr/local/lib"
export MAKEFLAGS="-j 5"

export RUBYOPT="-rubygems"

export PSPDEV="/opt/pspdev"
if [[ -s $PSPDEV ]]; then
    export PATH="$PATH:$PSPDEV/bin"
fi

zstyle ':completion:*' completer _complete _match _approximate _correct _expand
zstyle ':completion:*' original true
zstyle ':completion:*' completions 1
zstyle ':completion:*' file-sort name
zstyle ':completion:*' glob 1
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
#zstyle ':completion:*' max-errors 2
zstyle ':completion:*' prompt 'correction: %e '
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' substitute 1
zstyle ':completion:*' use-cache on

zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always

zstyle ':completion:*:cd:*' ignore-parents parent pwd

zstyle ':completion:*:rm:*' ignore-line yes
zstyle ':completion:*:mv:*' ignore-line yes
zstyle ':completion:*:cp:*' ignore-line yes

autoload -U compinit
compinit

typeset -g -A key

bindkey '^?' backward-delete-char
bindkey '^[[1~' beginning-of-line
bindkey '^[[5~' up-line-or-history
bindkey '^[[3~' delete-char
bindkey '^[[4~' end-of-line
bindkey '^[[6~' down-line-or-history
bindkey '^[[A' up-line-or-search
bindkey '^[[D' backward-char
bindkey '^[[B' down-line-or-search
bindkey '^[[C' forward-char

SSH_ENV="$HOME/.ssh/environment"

function start_agent {
  echo "Initializing new SSH agent..."
  /usr/bin/ssh-agent | sed 's/^echo/#echo/' >| "${SSH_ENV}"
  echo succeeded
  chmod 600 "${SSH_ENV}"
  . "${SSH_ENV}" > /dev/null
  /usr/bin/ssh-add;
}

if [ -f "${SSH_ENV}" ]; then
  . "${SSH_ENV}" > /dev/null
  ps ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
    start_agent;
  }
else
  start_agent;
fi

#alias ls='ls --color=auto -F'
alias grep='grep --color=auto'
alias f='find . | grep'
alias ..='cd ..'

alias weechat='weechat-curses'
alias rmpc='mpc --host=192.168.1.9'

alias tree="tree -C"

case `hostname` in
    arch-*) alias ls='ls --color';;
esac

compctl -/ cd

PROMPT=$'%B%F{}[%b%F{green}%~%F{}%B]==[%b%F{blue}%*%B%F{}]%F{red}==>%F{white}%b'

autoload -U zen

if [[ -s $HOME/.rvm/scripts/rvm ]]; then
    source $HOME/.rvm/scripts/rvm
fi

cr() { coderay $1 -term | less -r }

play-dl() {
    mplayer -cache 8192 -cache-min 4 $*
}

play-stream() {
    mplayer -playlist $1 -dumpstream -dumpfile /dev/fd/3 3>&1 1>&2 | tee $2 | mplayer -
}

to-android() {
    mencoder $1 -o $2 \
        -ovc lavc -oac lavc \
        -lavcopts vcodec=mpeg4:acodec=libfaac:vbitrate=800:abitrate=192:vglobal=1:aglobal=1 \
        -of lavf -lavfopts format=3gp \
        -noskip -noautoexpand -vf dsize=480:352:2,scale=-8:-8,harddup \
        -af channels=2,volnorm
}

md-indent() {
    xclip -o | ruby -pe '$_.prepend "    "' | xclip
}

md-unindent() {
    xclip -o | ruby -pe '$_[0, 4] = ""' | xclip
}

mount-iso() { # I always forget the syntax of that command
    sudo mount -o loop -t iso9660 "$1" /mnt/iso
}

sync-dir () {
    from="$1/"
    to="$2/"

    rsync -avurP "$from" "$to" || return 1
    rsync -avurP "$to"   "$from" || return 1
}

sync-file () {
    from="$1"
    to="$2"

    rsync -avuP "$from" "$to" || return 1
    rsync -avuP "$to"   "$from" || return 1
}

source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main)

[[ -n "$TMUX" ]] && export TERM="screen-256color"
