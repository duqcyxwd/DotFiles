#!/bin/sh
# ZSH Utility fns


function hello(){ #{{{1
  echo "hello $@"
}

# A higher order fucntion safe for pipe and parameter parsing
# Idea is from https://yannesposito.com/Scratch/en/blog/Higher-order-function-in-zsh/
function mapf { #{{{1

  # Example
  # mapf hello a b c d e
  # echo a b c d e | xargs -n 1 | mapf hello

  local func_name=$1
  shift

  # Support parameter
  for data in $@; print -- $(eval $func_name $data)

  # Support pipe
  [ ! -t 0 ] && while read -r data; do
    print -- $(eval $func_name $data)
  done;
}
