#!/bin/bash
# stop when error
set -e

echo update apt
sudo apt update

echo fetch the system\' infos
export OS_CORENAME=$(lsb_release -a | grep Codename | awk '{print $2}')
export OS_DISTRIBUTOR=$(lsb_release -a | grep Description | awk '{print $2}')
echo using $OS_DISTRIBUTOR  $OS_CORENAME

echo Docker Hub login
docker login -e="$DOCKER_EMAIL" -u="$DOCKER_USERNAME" -p="$DOCKER_PASSWORD"

echo install haskell-stack
mkdir -p ~/.local/bin
export PATH=$HOME/.local/bin:$PATH
travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
echo setup system-ghc
stack config set system-ghc --global true

echo setting up ghc
export PATH=/opt/ghc/$GHC_VER/bin:$PATH
echo new PATH: $PATH
