alias ls='ls --color=auto'
alias emacs='emacsclient -t -a emacs -nw'

alias ..='cd ..'
alias ...='cd ../../'
alias ll='ls -hl'

alias xi='xclip -i -selection clipboard'
alias xp='xclip -o'
alias xpp='xclip -selection clipboard -o'

alias pjson='python -mjson.tool'
alias tx='tidy -xml'
alias sweep='find . \( -name "*~" -or -name "*.bak" -or -name "#*#" \) -exec rm -vfr {} \;'
alias ssh="TERM=xterm ssh"

alias emacs-daemon="/usr/bin/emacs --daemon"

alias reload-config=". ~/.zshrc"

alias gst="git status"
alias gch="git checkout"
alias gsp="git stash pop"
alias gss="git stash"
alias rm="trash"
alias untar='tar xvf'
alias e=emacs
alias vi=emacs
alias vim=emacs
alias cd..='cd ..'
