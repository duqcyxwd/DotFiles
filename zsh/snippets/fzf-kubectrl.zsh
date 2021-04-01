#!/bin/sh
#   DEVTOOL: : K8s
#     K8S: NAMESPACE CONTEXT {{{3
{
  # Set k8s ns
  set-ns() {
      echo $1 >$HOME/.kube/KUBE_NS
      export KUBE_NS=$1
      # echo "kubectl config set-context --current --namespace=$KUBE_NS"
      kubectl config set-context --current --namespace=$KUBE_NS
      echo "Current Namespace: " $KUBE_NS
      kube_env_update&
  }
  
  set-nsi() {
      set-ns $(kubectl get namespaces | awk 'NR>1' | fzf -0 | awk '{print $1}')
  }
  
  set-context() {
      if [ $# -eq 0 ]; then
          echo "Require cluster"
          return
      fi
  
      echo "Will change context to $@"
      local context=$@
      kubectl config use-context $context &&
        local ns=$(kubectl config view --minify | grep namespace | awk '{print $2}') &&
        set-ns $ns
      # kube_env_update&
  }
  
  set-contexti() {
    # kubectl config use-context $(kubectl config get-contexts  | awk 'NR>1' | fzf | awk '{print $2}')
    set-context $(kubectl config get-contexts  | awk 'NR>1' | fzf -0 | awk '{print $2}')
  }
  
}
#     K8S: scale deployment {{{3
# Useage: 
#   kgdi | ksd0
#   kubectl scale deployment $(kgdi) --replicas=1
{
  ksd0() { 
    # Support pipe
    while read data; 
    do; 
      kubectl scale deployment $data --replicas=0
    done; 
  }
  
  ksd1() { 
    # Support pipe
    while read data; 
    do; 
      kubectl scale deployment $data --replicas=1
    done; 
  }
  
}

#     K8S: NAMESPACE CLEAN {{{3
{
  # helm ls | grep $KUBE_NS | cut -f1 | hpurge && kdns $KUBE_NS
  hpurge() { 
    # Support pipe
    while read data; 
    do; 
      helm del --purge $data; 
    done; 
  }
  
  alias hdelp='helm del --purge'
}
#     K8S: SETUP ALIAS {{{3
{
  if [ -f $HOME/.kube/KUBE_NS ]; then
      export KUBE_NS=$(cat $HOME/.kube/KUBE_NS)
  fi
  
  alias kexecit='kubectl exec -it'
  unalias -m kgns

  alias ns=set-ns
  alias nsi=set-nsi
  
  alias context=set-context
  alias contexti=set-contexti

  alias ksd='kubectrl scale deployment'

  alias kgns='kubectl get namespaces --sort-by=.metadata.creationTimestamp'
  alias kgdi="kubectl get deployment | fzf --header-lines=1 | awk '{print \$1}'"
}




