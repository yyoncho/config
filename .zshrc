# Set up the prompt

# enable command not found detection
. /etc/zsh_command_not_found

autoload -Uz promptinit
promptinit
#prompt adam1

if [ -f .local_zshrc ] ;
then
    source .local_zshrc
fi

setopt histignorealldups sharehistory

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

bindkey '^[2' copy-prev-word
# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

source $HOME/.bash_aliases

# delete word configuration
autoload -U select-word-style
select-word-style bash

autoload -Uz copy-earlier-word
zle -N copy-earlier-word
bindkey "^[m" copy-earlier-word

imv() {
  local src dst
  for src; do
    [[ -e $src ]] || { print -u2 "$src does not exist"; continue }
    dst=$src
    vared dst
    [[ $src != $dst ]] && mkdir -p $dst:h && mv -n $src $dst
  done
}

bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

bindkey -s '\eu' '^Ucd ..; ls^M'
bindkey -s '\eg' '^Ugst^M'

 # home and end button
bindkey "^[[H" beginning-of-line
bindkey "^[[F" end-of-line

source ~/.env

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# This makes cd=pushd
setopt AUTO_PUSHD

# this will ignore multiple directories for the stack.  Useful?  I dunno.
setopt PUSHD_IGNORE_DUPS

zstyle ":completion:*:commands" rehash 1

setopt rmstarsilent

source ~/.zsh-syntax/zsh-syntax-highlighting.zsh

#make the cursor faster
xset r rate 200 60

export EDITOR="emacsclient -t -a emacs -nw"

# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

plugins=(git mvn screen svn)

source $ZSH/oh-my-zsh.sh
# quote previous word in single or double quote
autoload -U modify-current-argument

_quote-previous-word-in-double() {
    modify-current-argument '${(qqq)${(Q)ARG}}'
    zle vi-forward-blank-word
}
zle -N _quote-previous-word-in-double
bindkey '^[s' _quote-previous-word-in-double

alias fix-spaces="sed -i -e 's/[ \t]*$//' -e 's/\t/    /g'"
export PATH=$PATH:~/.bin

alias d='emacsclient -e '"'"'(dired "'"'"'`pwd`'"'"'")'"'"''
