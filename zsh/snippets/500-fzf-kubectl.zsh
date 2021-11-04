#!/bin/zsh

unalias kgpn
unalias kgns
unalias kgp

# unalias kgcmi

#     K8S: NAMESPACE/CONTEXT {{{1
  # Notes: runcached in fzf --ttl vs remove $RUNCACHED_CACHE_DIR
  # --ttl can't invalid the cache for preview window



  kdelnsi() { #{{{2
    # Prefilter with my name to avoid accident delete important namespace
    local namespace=$(nsi --query "chuan ")

    if [ "$namespace" = "" ]; then
      echo "No namespace from chuan found"

      return 0
    fi

    echo "Will delete namespace: $namespace"
    kubectl delete namespace $namespace

    # Update cached namespace list
    TTL=0 kgns_cached > /dev/null &|
  }


# }}}2
# KUBECTL get pod id and container ID ###{{{1

alias kgci=kgpci

# KUBECTL Logs #{{{1


alias klffi=klpi
alias klp=klpi
alias klfi="kli -f"
alias klci="kli -f"
alias klafi="klai -f"

kexeciti() { #{{{1

  read -r POD_ID CONTAINER_ID < <(kgpci)

  if [[ "$POD_ID" == "" || "$CONTAINER_ID" == "" ]] ; then
    return 1
  fi

  echo "kubectl exec -it -c $CONTAINER_ID $POD_ID $@"

  kubectl exec -it -c $CONTAINER_ID $POD_ID $@
}
#}}}1
# KUBECTL Secret {{{1
kgseci() { #{{{2
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

# KUBECTL configmap #{{{1
kgcmi() { #{{{2
  local RES=$( runcached_ns kubectl get configmaps | fzf_tp --header-lines=2 \
    --preview-window right \
    --preview "runcached_ns kubectl describe configmaps {1}" | awk '{print $1}')

  echo $RES
}

# KUBECTL statefulset #{{{1
# alias kgst="kubectl get statefulset"
kgssi() { #{{{2
  runcached_ns kubectl get statefulset --namespace $(kcgcn) --kubeconfig $KUBECONFIG\
    | fzf_tp --header-lines=2 \
    --bind "ctrl-r:reload(runcached_ns_clean && runcached_ns kubectl get statefulset --namespace $(kcgcn) --kubeconfig $KUBECONFIG)" \
    --preview-window right \
    --preview 'runcached_ns kubectl describe statefulset {1}' \
    | awk '{print $1}'
}

# KUBECTL Deployment #{{{1
kgdi() { #{{{2
  runcached_ns kubectl get deployment --namespace $(kcgcn) --kubeconfig $KUBECONFIG \
    | fzf_tp --header-lines=2 \
    --bind "ctrl-r:reload(runcached_ns_clean && runcached_ns kubectl get deployment --namespace $(kcgcn) --kubeconfig $KUBECONFIG)" \
    --preview-window right \
    --preview 'runcached_ns kubectl describe deployment {1}' \
    | awk '{print $1}'
}


kddi() { #{{{2
  local deploy=$(kgdi);
  if [ "$deploy" = "" ]; then
    return 0
  fi
  kubectl describe deployment $deploy
}

# KUBECTL service #{{{1
kgsi() { #{{{2
  runcached_ns kubectl get svc --namespace $(kcgcn) --kubeconfig $KUBECONFIG \
    | fzf_tp --header-lines=2 \
    --bind "ctrl-r:reload(runcached_ns_clean && runcached_ns kubectl get svc --namespace $(kcgcn) --kubeconfig $KUBECONFIG)" \
    --preview-window right \
    --preview 'runcached_ns kubectl describe svc {1}' \
    | awk '{print $1}'
}


#    K8S: scale deployment {{{1
# Useage:
#   kubectl scale deployment $(kgdi) --replicas=1
#   kgdi | ksd0
#   kgdi | xargs -n 1 kubectl scale deployment $@ --replicas=0
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

  ksss0() {
    # Kubectl scale stateful set
    # Support pipe
    while read data;
    do;
      kubectl scale statefulset $data --replicas=0
    done;
  }

  ksss1() {
    # Support pipe
    while read data;
    do;
      kubectl scale statefulset $data --replicas=1
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

  # use kcgc to get context
  alias contexti=set-contexti
  alias kcuci=set-contexti

  alias kgnsi=nsi
  alias snsi=set-nsi
  alias kgns='kgns_cached'
  alias ns='kgns_cached'

  alias kgsec=kube_secret_preview
  alias kgsecl="runcached_ns kubectl get secret --namespace $(kcgcn) --kubeconfig $KUBECONFIG"

  # alias ksd='kubectl scale deployment'


  alias kexecit='kubectl exec -it'
  # alias kgp='kgp_cached'




}
