#!/bin/bash

LSFILE=~/temp/helm-release/ls-$(date +%y-%m-%d_%H:%M)
helm ls -d >> $LSFILE 
cp $LSFILE ~/temp/helm-release/helm-ls
