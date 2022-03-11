# Usability enhancements
alias vim='vim -p'
alias ...='cd ../../..'
alias ..='cd ..'
alias cd..='cd ..'
alias bat='batcat'
alias diff='diff -u'
alias got='git'
alias less="less -r -R"
alias igrep="grep -i"

# Custom commands
alias myextip='curl ifconfig.me'

# System parts
alias scs='systemctl status'
alias scre='systemctl restart'

# AWS related
alias awe='aws-vault exec'
alias awl='aws-vault list'

#docker
alias d='docker'
alias dps='docker ps'
alias dpsa='docker ps -a'
alias drm='docker rm'
alias drmi='docker rmi'

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

# Terraform
alias tfinit='terraform init'
alias tfplan='terraform plan'
alias tfapply='terraform apply'
alias tfdestroy='terraform destroy'
alias tfmt='terraform fmt'
alias tfst='terraform state'

# Diligent-related
alias btssh='btsshs'
alias bts='btsshs'
#alias prod='btssh sec11p0deploy11'
alias prod='bts sec11p0deploy11'
