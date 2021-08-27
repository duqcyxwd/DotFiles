#!/bin/sh

unalias kgpn
unalias kgp
unalias kgns

# unalias kgcmi

#     K8S: NAMESPACE/CONTEXT {{{1
#     TODO Deprecate this one
export CURRENT_KUBE_NS_FILE=$HOME/.kube/KUBE_NS

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
    #   fzf_tp --info=inline --layout=reverse --header-lines=1 \
    #   --prompt "$(kubectl config current-context | sed 's/-context$//')> " \
    #   --bind "ctrl-r:reload(cat $CURRENT_KUBE_NS_LIST_FILE)" \
    #   --preview-window right \
    #   --preview "kube_namespace_preview {1} $KUBECONFIG" "$@" \
    #   | awk '{print $1}')
    # echo $token


    # # Method 6, The one caches!!!!
    # local token=$({ cat $CURRENT_KUBE_NS_LIST_FILE && kgns-cached }  |
    #   fzf_tp --info=inline --layout=reverse --header-lines=1 \
    #   --prompt "$(kubectl config current-context | sed 's/-context$//')> " \
    #   --bind "ctrl-r:reload(/bin/rm -rf $RUNCACHED_CACHE_DIR && cat $CURRENT_KUBE_NS_LIST_FILE)" \
    #   --preview-window right \
    #   --preview "runcached --prune-cache kube_namespace_preview {1} $KUBECONFIG" "$@" \
    #   | awk '{print $1}');
    # echo $token


    # Method 7, The one caches!!!!
    local CACHE_DIR=$RUNCACHED_CACHE_DIR/$(kube_cache_key)

    local token=$( kgns_cached |
      fzf_tp --info=inline --layout=reverse --header-lines=2 \
      --prompt "$(kubectl config current-context | sed 's/-context$//')> " \
      --bind "ctrl-r:reload(/bin/rm -rf $CACHE_DIR && kgns_cached)" \
      --preview-window right \
      --preview "runcached --ignore-pwd --ignore-env --cache-dir $CACHE_DIR kube_namespace_preview {1} $KUBECONFIG" "$@" \
      | awk '{print $1}');

    echo $token

  }

  # Notes: runcached in fzf --ttl vs remove $RUNCACHED_CACHE_DIR
  # --ttl can't invalid the cache for preview window


  set-nsi() { #{{{2
    set-ns $( nsi )
  }

  kdelnsi() { #{{{2
    # Similar with kgns_cached with grep chuan
    # set-ns $({{head -n 1 $CURRENT_KUBE_NS_LIST_FILE && cat $CURRENT_KUBE_NS_LIST_FILE | grep chuan} && kgns-cached } | fzf +m \

    local CACHE_DIR=$RUNCACHED_CACHE_DIR/$(kube_cache_key)

    local namespace=$( { kgns_cached | grep -e chuan -e STATUS } |
      fzf_tp --info=inline --layout=reverse --header-lines=1 \
      --prompt "$(kubectl config current-context | sed 's/-context$//')> " \
      --bind "ctrl-r:reload(/bin/rm -rf $CACHE_DIR && kgns_cached | head -n 1 && kgns_cached | grep chuan)" \
      --preview-window right \
      --preview "runcached --ignore-pwd --ignore-env --cache-dir $CACHE_DIR kube_namespace_preview {1} $KUBECONFIG" "$@" \
      | awk '{print $1}');

    if [ "$namespace" = "" ]; then
      echo "No namespace from chuan found"

      return 0
    fi

    echo "Will delete namespace: $namespace"
    kubectl delete namespace $namespace

    # Update cached namespace list
    TTL=0 kgns_cached > /dev/null &|

    # kubectl delete namespace $( kgns_cached | grep chuan | fzf +m \
    # --bind "ctrl-r:reload(/bin/rm -rf $CACHE_DIR && kgns_cached | grep chuan)" \
    # --preview "kube_namespace_preview {1} $KUBECONFIG" "$@" \
    # --header-lines=1 -0 | awk '{print $1}')

  }


  set-contexti() { #{{{2
    # kubectl config use-context $(kubectl config get-contexts  | awk 'NR>1' | fzf | awk '{print $2}')
    set-context $(kubectl config get-contexts | fzf-tmux -p +m -0 --header-lines=1 | awk '{print $2}')
  }



kgpi() { #{{{1
  # No need to clean kdp_cached since ttl is 15 second. I can add job to clean pod cache dir
  # in future if it is needed.

  kgp_cached|
      fzf_tp --info=inline --layout=reverse --header-lines=2 \
      --bind "ctrl-r:reload(kgp_cached 0 $(kcgcn))" \
      --prompt "$(kubectl config current-context | sed 's/-context$//')/$(kcgcn) pods> " \
      --preview-window right \
      --preview 'kdp_cached {1} 30' \
      "$@" \
      | awk '{print $1}'
}


# KUBECTL Logs #{{{1
# kli : kube logs
# klfp kube logs preview
# klai kube logs all container

klpi() { #{{{2
  # k log preview interactive
  kgp_cached|
    FZF_TP_OPTS="-p 100%" \
      fzf_tp --info=inline --layout=reverse --header-lines=2 \
      --bind "ctrl-r:reload(kgp_cached 0 $(kcgcn))" \
      --prompt "$(kubectl config current-context | sed 's/-context$//')/$(kcgcn) pods> " \
      --preview-window "bottom:85%" \
      --preview-window "border-top" \
      --preview-window "follow" \
      --preview 'kubectl logs --all-containers -f {1}' \
      "$@" \
      | awk '{print $1}'
}

kli() { #{{{2

  POD_ID=$(kgpi)

  if [[ "$POD_ID" == "" ]] ; then
    return 1
  fi

  CONTAINER_ID=$(runcached kubectl get pod $POD_ID -o jsonpath="{.spec.containers[*].name}" \
    | xargs -n 1 \
    | fzf_tp -1 \
      --preview-window "bottom:85%" \
      --preview-window "border-top" \
      --preview-window "follow" \
      --preview "kubectl logs -c {1} -f $POD_ID" \
      --prompt "container: ")


  if [[ "$CONTAINER_ID" == "" ]] ; then
    return 1
  fi

  echo "POD ID: $POD_ID"
  echo "Container ID: $CONTAINER_ID"

  kubectl logs -c $CONTAINER_ID $@ $POD_ID
}

klai() { #{{{2

  POD_ID=$(kgpi)

  if [[ "$POD_ID" == "" ]] ; then
    return 1
  fi

  echo "POD ID: $POD_ID"

  kubectl logs --all-containers $@ $POD_ID
}


# }}}2

alias klffi=klpi
alias klp=klpi
alias klfi="kli -f"
alias klci="kli -f"
alias klafi="klai -f"

#}}}1
kgseci() { #{{{1
  # Get and preview secre
  # CACHE_DIR is namespaced based, so ctrl-r will not updates others
  local CACHE_DIR=$RUNCACHED_CACHE_DIR/$(kube_cache_key $(kcgcn))

  local SEC=$( runcached_ns kubectl get secret --namespace $(kcgcn) --kubeconfig $KUBECONFIG|
      fzf_tp --info=inline --layout=reverse --header-lines=2 \
      --bind "ctrl-r:reload(runcached_ns_clean && runcached_ns --ttl 0 kubectl get secret --namespace $(kcgcn) --kubeconfig $KUBECONFIG)" \
      --prompt "$(kubectl config current-context | sed 's/-context$//')> " \
      --preview-window right \
      --preview "runcached_ns kube_secret_preview {1} $KUBECONFIG" "$@" \
      | awk '{print $1}' )

  echo $SEC
  # runcached --ignore-pwd --ignore-env --cache-dir $CACHE_DIR kube_secret_preview $SEC $KUBECONFIG
}


kgcmi() { #{{{1
  local RES=$( runcached_ns kubectl get configmaps | fzf_tp --header-lines=2 \
    --preview-window right \
    --preview "runcached_ns kubectl describe configmaps {1}" | awk '{print $1}')

  echo $RES
}

kgdi() { #{{{1
  runcached_ns kubectl get deployment | fzf_tp --header-lines=2 --preview-window right --preview 'runcached_ns kubectl describe deployment {1}' | awk '{print $1}'
}

kddi() { #{{{1
  local deploy=$(kgdi);
  if [ "$deploy" = "" ]; then
    return 0
  fi
  kubectl describe deployment $deploy
}

#    K8S: scale deployment {{{1
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

# alias kgdi="kubectl get deployment | fzf_tp --header-lines=1 --preview-window right --preview 'kubectl describe deployment {1}' | awk '{print \$1}'"
#     K8S: SETUP ALIAS {{{1
{
  if [ -f $CURRENT_KUBE_NS_FILE ]; then
    export KUBE_NS=$(cat $CURRENT_KUBE_NS_FILE)
  fi

  # use kcgc to get context
  alias contexti=set-contexti
  alias kcuci=set-contexti

  alias kgnsi=nsi
  alias ns='kgns'
  alias snsi=set-nsi

  alias kgsec=kube_secret_preview
  alias kgsecl="runcached_ns kubectl get secret --namespace $(kcgcn) --kubeconfig $KUBECONFIG"

  # alias ksd='kubectl scale deployment'


  alias kexecit='kubectl exec -it'
  alias kgns='kgns_cached'
  # alias kgp='kgp_cached'




}
