eval "$(/opt/homebrew/bin/brew shellenv)"

export GPG_TTY="$(tty)"

export "SSH_AUTH_SOCK=${HOME}/.gnupg/S.gpg-agent.ssh"
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

gpgconf --launch gpg-agent

export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced
export PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
