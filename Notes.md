# DotFiles

Custom configurations and settings

# Setup for Mac

- Create [ssh key](https://help.github.com/en/enterprise/2.17/user/authenticating-to-github/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent)
- Install [Brew](https://brew.sh/)
- Install Useful tools

```sh

curl https://raw.githubusercontent.com/so-fancy/diff-so-fancy/master/third_party/build_fatpack/diff-so-fancy >> $HOME/script/diff-so-fancy
chmod 777 diff-so-fancy

```

## Install docker

https://docs.docker.com/docker-for-mac/edge-release-notes/
https://download.docker.com/mac/edge/30090/Docker.dmg

## ZSH Shell

- install [oh-my-zsh](https://github.com/ohmyzsh/ohmyzsh)

  - `sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"`


```sh
mkdir ~/script
cd ~/script

git clone https://github.com/tmuxinator/tmuxinator.git
git clone https://github.com/Dabz/kafka-zsh-completions.git


```

#### Install Powerline font

https://github.com/powerline/fonts

#### Install other scripts

```sh

python < <(curl -s https://bootstrap.pypa.io/get-pip.py)
pip install psutil #for paulmelnikow/zsh-startup-timer

```
---

### Wildfly

https://wildfly.org/downloads/



