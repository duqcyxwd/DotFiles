#!/bin/zsh

unalias kgpn 2>/dev/null || true
unalias kgns 2>/dev/null || true
unalias kgp 2>/dev/null || true
unalias kgsec 2>/dev/null || true
unalias kgpw 2>/dev/null || true

FZF_KUBECTL_DEFAULT_OPTS=" --exit-0 --info=inline --layout=reverse"

# unalias kgcmi

#     K8S: NAMESPACE/CONTEXT {{{1
# Notes: runcached in fzf --ttl vs remove $RUNCACHED_CACHE_DIR
# --ttl can't invalid the cache for preview window


kdelnsi() { #{{{2
  # Prefilter with my name to avoid accident delete important namespace
  local namespace=$(nsi --query "swordform ")
  if [ "$namespace" = "" ]; then
    echo "No namespace found"
    return 0
  fi
  echo "Will delete namespace: $namespace"
  kubectl delete namespace $namespace
  # Update cached namespace list
  TTL=0 kgns_cached > /dev/null &|
}


# }}}2

# KUBECTL Secret {{{1
kgseci() { #{{{2
  # Get and preview secre
  # CACHE_DIR is namespaced based, so ctrl-r will not updates others
  local CACHE_DIR=$RUNCACHED_CACHE_DIR/$(kube_cache_key $(kcgcn))

  local SEC=$( runcached_ns kubectl get secret --namespace $(kcgcn) --kubeconfig $KUBECONFIG|
    FZF_DEFAULT_OPTS=" $FZF_DEFAULT_OPTS $FZF_KUBECTL_DEFAULT_OPTS" \
      fzf_tp --header-lines=2 \
      --bind "ctrl-r:reload(runcached_ns kubectl get secret --namespace $(kcgcn) --kubeconfig $KUBECONFIG)" \
      --prompt "$(kubectl config current-context | sed 's/-context$//')> " \
      --preview-window right \
      --preview "runcached_ns kube_secret_preview {1} $KUBECONFIG" "$@" \
      | awk '{print $1}' )

  echo $SEC
  # runcached --ignore-pwd --ignore-env --cache-dir $CACHE_DIR kube_secret_preview $SEC $KUBECONFIG
}

kgsec() { #{{{2
  if [ "$1" != "" ]; then
    local SEC="$1"
  else
    local SEC=$(kgseci)
  fi

  if [ "$SEC" = "" ]; then
    return 0
  fi
  kube_secret_preview $SEC
}

# KUBECTL configmap #{{{1
kgcmi() { #{{{2
  local RES=$( runcached_ns kubectl get configmaps | fzf_tp --header-lines=2 \
    --preview-window right \
    --preview "runcached_ns kubectl describe configmaps {1}" | awk '{print $1}')

  echo $RES
}

alias kdcmi='kubectl describe configmap $(kgcmi)'
alias kgcmdi=kdcmi

# KUBECTL statefulset #{{{1
# alias kgst="kubectl get statefulset"
kgssi() { #{{{2
  runcached_ns kubectl get statefulset --namespace $(kcgcn) --kubeconfig $KUBECONFIG\
    | fzf_tp --header-lines=2 \
    --bind "ctrl-r:reload(runcached_ns kubectl get statefulset --namespace $(kcgcn) --kubeconfig $KUBECONFIG)" \
    --preview-window right \
    --preview 'runcached_ns kubectl describe statefulset {1}' \
    | awk '{print $1}'
}

# KUBECTL Deployment #{{{1
kgdi() { #{{{2
  runcached_ns kubectl get deployment --namespace $(kcgcn) --kubeconfig $KUBECONFIG \
    | fzf_tp --header-lines=2 \
    --bind "ctrl-r:reload(runcached_ns kubectl get deployment --namespace $(kcgcn) --kubeconfig $KUBECONFIG)" \
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

kgdpi() { #{{{2
  # Return pod id by given deployment name.
  if [ "$1" -eq "" ]; then
    deployment=$(kgdi)
  else
    deployment=$1
  fi

  if [[ "$deployment" == "" ]]; then
    return 0
  fi

  pods="$(kgp_filter $deployment)"
  count="$(echo $pods | wc -l)"

  if [ $count -eq 3 ]; then
    echo "$pods" | tail -1
    return
  fi


  kgp_filter $deployment | fzf_tp --select-1 --info=inline --layout=reverse --header-lines=2 \
        --bind "ctrl-r:reload(KGP_TTL=0 kgp_filter $deployment)" \
        --bind="ctrl-y:execute-silent(echo {1} | tr-newline | pbcopy )" \
        --prompt "$(kcgccn) pods> " \
        --preview-window "right:75%" \
        --preview 'kdp_cached {1} 50' \
}


# KUBECTL service #{{{1
kgsi() { #{{{2
  runcached_ns kubectl get svc --namespace $(kcgcn) --kubeconfig $KUBECONFIG \
    | fzf_tp --header-lines=2 \
    --bind "ctrl-r:reload(runcached_ns kubectl get svc --namespace $(kcgcn) --kubeconfig $KUBECONFIG)" \
    --preview-window right \
    --preview 'runcached_ns kubectl describe svc {1}' \
    | awk '{print $1}'
}


# KUBECTL Ingress #{{{1
kgii() { #{{{2
  runcached_ns kubectl get ingress --namespace $(kcgcn) --kubeconfig $KUBECONFIG \
    | fzf_tp --header-lines=2 \
    --bind "ctrl-r:reload(runcached_ns kubectl get ingress --namespace $(kcgcn) --kubeconfig $KUBECONFIG)" \
    --preview-window right \
    --preview 'runcached_ns kubectl describe ingress {1}' \
    | awk '{print $1}'
}


kdii() { #{{{2
  local ingress=$(kgii);
  if [ "$ingress" = "" ]; then
    return 0
  fi
  kubectl describe ingress $ingress
}

#    K8S: scale deployment {{{1
# Useage:
#   kubectl scale deployment $(kgdi) --replicas=1
#   kgdi | ksd0
#   kgdi | xargs -n 1 kubectl scale deployment $@ --replicas=0
{

  # kgdi | map ksdr
  ksdr() {
    echo "Restart deployment $@"
    kubectl scale deployment $@ --replicas=0
    kubectl scale deployment $@ --replicas=1
  }

  ksssr() {
    echo "Restart statefull set $@"
    kubectl scale statefulset $@ --replicas=0
    kubectl scale statefulset $@ --replicas=1
  }

  ksd0() {
    # only Support pipe
    while read data;
    do;
      kubectl scale deployment $data --replicas=0
    done;
  }

  ksd1() {
    # only Support pipe
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
  alias ctxi=set-contexti
  alias ctx=set-context
  alias kcuci=set-contexti

  alias kgnsi=nsi
  alias snsi=set-nsi
  alias kgns='kgns_cached'
  alias ns='kgns_cached'

  alias kgsecl="runcached_ns kubectl get secret --namespace $(kcgcn) --kubeconfig $KUBECONFIG"

  # Produce a period-delimited tree of all keys returned for pods, etc
  alias kpaths="kubectl get pods -o json | jq -c 'paths|join(\".\")'"

  # alias ksd='kubectl scale deployment'


  alias kexecit='kubectl exec -it'
  alias kge='kubectl get events'
  alias kgew='kubectl get events --watch'
  alias kgp='kgp_cached'
  alias kgpr='/usr/local/bin/kubectl get pods'
  alias kgpw='/usr/local/bin/kubectl get pods --watch'
  alias ke='kubectl edit'
  alias ked='kubectl edit deployment'
  alias kec='kubectl edit configmap'

}
