#!/bin/sh
#   DEVTOOL: : K8s
#     K8S: NAMESPACE CONTEXT {{{1
export CURRENT_KUBE_NS_FILE=$HOME/.kube/KUBE_NS
export CURRENT_KUBE_NS_LIST_FILE=$HOME/.kube/KUBE_NS_LIST

{
  # Set k8s ns
  set-ns() {
      if [ $# -eq 0 ]; then
          mlog "[set-ns] Require namespace"
          return
      fi

      echo $1 > $CURRENT_KUBE_NS_FILE
      export KUBE_NS=$1
      kubectl config set-context --current --namespace=$KUBE_NS
      echo "Current Namespace: " $KUBE_NS

      if typeset -f kube_env_update > /dev/null; then
        # Env update will be picked up by p10k them post load
        # setopt LOCAL_OPTIONS no_monitor
        kube_env_update &|
      fi
  }

  set-nsi() {
    # change event will wait for input pipe finish
    
    # setopt no_notify no_monitor
    # setopt LOCAL_OPTIONS no_monitor

    # Method 1, precached and reload on type
    # kgns-cached &|
    # set-ns $(cat $CURRENT_KUBE_NS_LIST_FILE | fzf --bind "change:reload:cat $CURRENT_KUBE_NS_LIST_FILE || true" \
    # --bind "ctrl-r:reload(cat $CURRENT_KUBE_NS_LIST_FILE)" --header-lines=1 -0 | awk '{print $1}')

    # Method 2, manual reload
    # set-ns $(cat $CURRENT_KUBE_NS_LIST_FILE | fzf --bind "ctrl-r:reload(kubectl get namespaces --sort-by=.metadata.creationTimestamp)" --header-lines=1 -0 | awk '{print $1}')
    # kgns-cached &|
    
    # Method 3, mix, preload and reload on demand
    # kgns-cached &|
    # set-ns $(cat $CURRENT_KUBE_NS_LIST_FILE | fzf \
    # --bind "ctrl-r:reload(cat $CURRENT_KUBE_NS_LIST_FILE)" --header-lines=1 -0 | awk '{print $1}')

    # Method 4, manual reload with loading icon
    set-ns $({ cat $CURRENT_KUBE_NS_LIST_FILE && kgns-cached } | fzf +m \
    --bind "ctrl-r:reload(cat $CURRENT_KUBE_NS_LIST_FILE)" --header-lines=1 -0 | awk '{print $1}')
  }
  
  set-context() {
      if [ $# -eq 0 ]; then
          mlog "[set-context] Require a context"
          return
      fi
  
      # echo "Will change context to $@"
      local context=$@
      kubectl config use-context $context
      
      kgns-cached &|

      local ns=$(kubectl config view --minify | grep namespace | awk '{print $2}') &&
        set-ns $ns

      
  }
  
  set-contexti() {
    # kubectl config use-context $(kubectl config get-contexts  | awk 'NR>1' | fzf | awk '{print $2}')
    set-context $(kubectl config get-contexts | fzf +m -0 --header-lines=1 | awk '{print $2}')
  }
  
}
#     K8S: scale deployment {{{1
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

#     K8S: NAMESPACE CLEAN {{{1
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
#     K8S: SETUP ALIAS {{{1
{
  if [ -f $CURRENT_KUBE_NS_FILE ]; then
      export KUBE_NS=$(cat $CURRENT_KUBE_NS_FILE)
  fi
  
  unalias -m kgns

  alias kexecit='kubectl exec -it'

  alias ns=set-ns
  alias nsi=set-nsi
  
  alias context=set-context
  alias contexti=set-contexti

  alias ksd='kubectrl scale deployment'

  alias kgns='kubectl get namespaces --sort-by=.metadata.creationTimestamp'
  alias kgdi="kubectl get deployment | fzf --header-lines=1 | awk '{print \$1}'"
}




