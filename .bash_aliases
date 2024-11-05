# bash aliases
# vim:ft=sh

alias ydl="yt-dlp --cookie $HOME/Videos/my/youtube -f best[ext=mp4]"
alias winediscard="WINEPREFIX=~/.local/wine/discardablewine wine"

# Usability enhancements
alias vim='vim -p'
alias ...='cd ../../..'
alias ..='cd ..'
alias cd..='cd ..'
alias bat='batcat'
#alias cat='batcat'
alias diff='diff -u'
alias got='git'
alias less="less -r -R"

# Custom commands
alias myextip='curl ifconfig.me'
alias lsblk="lsblk|grep -v loop"
alias blkid="blkid|grep -v loop"

# System parts
alias scs='systemctl status'
alias scre='systemctl restart'

# Git
alias gcl="git clone"
alias gco="git checkout"

# AWS related
alias awe='aws-vault exec'
alias awl='aws-vault list'

# Docker
alias dps="docker ps"
alias dpsa="docker ps -a"

# Kubernetes
alias ks="kubeswitch"
alias kctl="kubectl"
alias k="kubectl"
alias kg='kubectl get'
alias kd='kubectl describe'
alias kswc='kubeswitch ctx'
alias kswn='kubeswitch ns'
alias ke='kubectl edit'
alias ked='kubectl edit deploy'
alias kg='kubectl get'
alias kgcm='kubectl get configmap'
alias kgp='kubectl get pod'
alias kgpo='kubectl get pods'
alias kgd='kubectl get deploy'
alias kgds='kubectl get ds'
alias kgs='kubectl get secret'
alias kgss='kubectl get statefulset'
alias kd='kubectl describe'
alias kdel='kubectl delete'
alias kdele='kubectl get po --all-namespaces --field-selector "status.phase==Failed" -o json | kubectl delete -f -'
alias kl='kubectl logs'
alias kgcrb='kubectl get clusterrolebinding'
alias kgcr='kubectl get clusterrole'
alias kcr='kubectl create'
