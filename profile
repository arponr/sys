export PATH=/usr/local/bin:$PATH

export GOPATH=$HOME/Documents/code/go
export GOROOT=/usr/local/opt/go/libexec
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$GOROOT/bin

export NODE_PATH=/usr/local/lib/node_modules

export PATH=$PATH:/Applications/Postgres.app/Contents/Versions/9.4/bin

export PS1="\W $ "
export TERM=xterm-256color

alias e='emacsclient'
export EDITOR='emacsclient'

alias merge='/System/Library/Automator/Combine\ PDF\ Pages.action/Contents/Resources/join.py'

lmk() {
  latexmk -pvc -pdf $1; latexmk -c $1;
}
