# DotFiles

Custom configurations and settings

# Setup for Mac

- Create [ssh key](https://help.github.com/en/enterprise/2.17/user/authenticating-to-github/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent)
- Install [Brew](https://brew.sh/)
- Install Useful tools

```sh

# brew install emacs
brew cask install emacs
brew install molovo/revolver/revolver

brew install zookeeper #zkcli
# Better zkcli https://github.com/let-us-go/zkcli
# brew tap let-us-go/zkcli
# brew install zkcli

brew install cassandra

brew tap AdoptOpenJDK/openjdk
brew cask install adoptopenjdk8


# https://stackoverflow.com/questions/24342886/how-to-install-java-8-on-mac
brew cask install adoptopenjdk/openjdk/adoptopenjdk8
# Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk
export JAVA_HOME=/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home


#export M2_HOME=/usr/local/Cellar/maven/3.5.0/libexec
#export M2=${M2_HOME}/bin
#export PATH=${PATH}:${M2_HOME}/bin


brew cask install graphiql
# hotoo/gitopen
npm install -g gitopen
npm install --global git-recent

curl https://raw.githubusercontent.com/so-fancy/diff-so-fancy/master/third_party/build_fatpack/diff-so-fancy >> $HOME/script/diff-so-fancy
chmod 777 diff-so-fancy


#UI stuff
npm install --global gulp-cli
```

## Install docker

https://docs.docker.com/docker-for-mac/edge-release-notes/
https://download.docker.com/mac/edge/30090/Docker.dmg

## ZSH Shell

- install [oh-my-zsh](https://github.com/ohmyzsh/ohmyzsh)

  - `sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"`

-

```sh
mkdir ~/script
cd ~/script

curl -L git.io/antigen > antigen.zsh
git clone https://github.com/mafredri/zsh-async.git
git clone https://github.com/tmuxinator/tmuxinator.git
git clone https://github.com/Dabz/kafka-zsh-completions.git


```

#### Install Powerline font

https://github.com/powerline/fonts

#### Install other scripts

```sh
brew install fzf
$(brew --prefix)/opt/fzf/install

brew tap "rhysd/git-brws" "https://github.com/rhysd/git-brws"
brew install git-brws

python < <(curl -s https://bootstrap.pypa.io/get-pip.py)
pip install psutil #for paulmelnikow/zsh-startup-timer

```
---

### Wildfly

https://wildfly.org/downloads/



