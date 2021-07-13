#!/bin/sh
#     K8S: NAMESPACE CONTEXT {{{1
export CURRENT_KUBE_NS_FILE=$HOME/.kube/KUBE_NS
export CURRENT_KUBE_NS_LIST_FILE=$HOME/.kube/KUBE_NS_LIST

unalias kgpn
unalias kgns

{
  # Set k8s ns
nsi() { #{{{2
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

    # # Method 4, manual reload with loading icon
    # set-ns $({ cat $CURRENT_KUBE_NS_LIST_FILE && kgns-cached } | fzf +m \
    # --bind "ctrl-r:reload(cat $CURRENT_KUBE_NS_LIST_FILE)" --header-lines=1 -0 | awk '{print $1}')

    # # # Method 5, new one with preview!
    # local token=$({ cat $CURRENT_KUBE_NS_LIST_FILE && kgns-cached }  |
    #   fzf-tmux -p 85% --info=inline --layout=reverse --header-lines=1 \
    #   --prompt "$(kubectl config current-context | sed 's/-context$//')> " \
    #   --bind "ctrl-r:reload(cat $CURRENT_KUBE_NS_LIST_FILE)" \
    #   --preview-window right \
    #   --preview "kube_namespace_preview {1} $KUBECONFIG" "$@" \
    #   | awk '{print $1}')
    # echo $token


    # # Method 6, The one caches!!!!
    # local token=$({ cat $CURRENT_KUBE_NS_LIST_FILE && kgns-cached }  |
    #   fzf-tmux -p 85% --info=inline --layout=reverse --header-lines=1 \
    #   --prompt "$(kubectl config current-context | sed 's/-context$//')> " \
    #   --bind "ctrl-r:reload(/bin/rm -rf $RUNCACHED_CACHE_DIR && cat $CURRENT_KUBE_NS_LIST_FILE)" \
    #   --preview-window right \
    #   --preview "runcached --prune-cache kube_namespace_preview {1} $KUBECONFIG" "$@" \
    #   | awk '{print $1}');
    # echo $token


    # Method 7, The one caches!!!!
    local CACHE_DIR=$RUNCACHED_CACHE_DIR/$(kube_cache_key)

    local token=$( kgns_cached |
      fzf-tmux -p 85% --info=inline --layout=reverse --header-lines=1 \
      --prompt "$(kubectl config current-context | sed 's/-context$//')> " \
      --bind "ctrl-r:reload(/bin/rm -rf $CACHE_DIR && kgns_cached)" \
      --preview-window right \
      --preview "runcached --ignore-pwd --ignore-env --cache-dir $CACHE_DIR kube_namespace_preview {1} $KUBECONFIG" "$@" \
      | awk '{print $1}');

    echo $token

  }

  # Notes: runcached in fzf --ttl vs remove $RUNCACHED_CACHE_DIR
  # --ttl can't invalid the cache for preview window

  set-ns() { #{{{2
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

  set-nsi() { #{{{2
    set-ns $( nsi )
  }

  kdelnsi() { #{{{2
    # set-ns $({{head -n 1 $CURRENT_KUBE_NS_LIST_FILE && cat $CURRENT_KUBE_NS_LIST_FILE | grep chuan} && kgns-cached } | fzf +m \

    local CACHE_DIR=$RUNCACHED_CACHE_DIR/$(kube_cache_key)
    local namespace=$( { kgns_cached | head -n 1 && kgns_cached | grep chuan } |
      fzf_tp --info=inline --layout=reverse --header-lines=1 \
      --prompt "$(kubectl config current-context | sed 's/-context$//')> " \
      --bind "ctrl-r:reload(/bin/rm -rf $CACHE_DIR && kgns_cached | head -n 1 && kgns_cached | grep chuan)" \
      --preview-window right \
      --preview "runcached --cache-dir $CACHE_DIR --prune-cache kube_namespace_preview {1} $KUBECONFIG" "$@" \
      | awk '{print $1}');

    if [ "$namespace" -eq "" ]; then
      echo "No cached namespace from chuan"
      echo "Current namespace list:"
      kgns_cached 0
      return 0
    fi

    echo "Will delete namespace: $namespace"
    kubectl delete namespace $namespace

    # kubectl delete namespace $( kgns_cached | grep chuan | fzf +m \
    # --bind "ctrl-r:reload(/bin/rm -rf $CACHE_DIR && kgns_cached | grep chuan)" \
    # --preview "kube_namespace_preview {1} $KUBECONFIG" "$@" \
    # --header-lines=1 -0 | awk '{print $1}')


  }

  set-context() { #{{{2
      if [ $# -eq 0 ]; then
          mlog "[set-context] Require a context"
          return
      fi

      # echo "Will change context to $@"
      local context=$@
      kubectl config use-context $context

      # kgns-cached &|

      local ns=$(kubectl config view --minify | grep namespace | awk '{print $2}') && set-ns $ns
  }

  set-contexti() { #{{{2
    # kubectl config use-context $(kubectl config get-contexts  | awk 'NR>1' | fzf | awk '{print $2}')
    set-context $(kubectl config get-contexts | fzf-tmux -p +m -0 --header-lines=1 | awk '{print $2}')
  }

}

kgseci() { #{{{1
  # Get and preview secre

  # kubectl config use-context $(kubectl config get-contexts  | awk 'NR>1' | fzf | awk '{print $2}')
  # WIP, use ctrl-r to reload current list and current secret preview, need refresh preview to see change
  # runcached --ignore-env kubectl get secret --namespace $(kcgcn) --kubeconfig $KUBECONFIG|
  #     fzf-tmux -p 85% --info=inline --layout=reverse --header-lines=1 \
  #     --bind "ctrl-r:reload(runcached --ignore-env --ttl 0 kube_secret_preview {1} $KUBECONFIG 1&> /dev/null && runcached --ignore-env --ttl 0 kubectl get secret --namespace $(kcgcn) --kubeconfig $KUBECONFIG)" \
  #     --prompt "$(kubectl config current-context | sed 's/-context$//')> " \
  #     --preview-window right \
  #     --preview "runcached --ignore-env kube_secret_preview {1} $KUBECONFIG" "$@" \
  #     | awk '{print $1}'

  # CACHE_DIR is namespaced based, so ctrl-r will not updates others
  local CACHE_DIR=$RUNCACHED_CACHE_DIR/$(kube_cache_key $(kcgcn))
  echo $CACHE_DIR

  runcached --ignore-pwd --ignore-env --cache-dir $CACHE_DIR kubectl get secret --namespace $(kcgcn) --kubeconfig $KUBECONFIG|
      fzf-tmux -p 85% --info=inline --layout=reverse --header-lines=1 \
      --bind "ctrl-r:reload(/bin/rm -rf $CACHE_DIR && kubectl get secret --namespace $(kcgcn) --kubeconfig $KUBECONFIG)" \
      --prompt "$(kubectl config current-context | sed 's/-context$//')> " \
      --preview-window right \
      --preview "runcached --ignore-pwd --ignore-env --cache-dir $CACHE_DIR kube_secret_preview {1} $KUBECONFIG" "$@" \
      | awk '{print $1}'
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


  alias kexecit='kubectl exec -it'

  alias nsl="cat $CURRENT_KUBE_NS_LIST_FILE"
  # alias nsi=set-nsi

  # use kcgc to get context
  alias contexti=set-contexti
  alias kcuci=set-contexti

  # alias ksd='kubectl scale deployment'

  alias kgdi="kubectl get deployment | fzf-tmux -p --header-lines=1 | awk '{print \$1}'"
  alias kgns='kgns_cached'
  alias kgnsi=nsi
  alias ns='kgns'
  alias snsi=set-nsi
}





