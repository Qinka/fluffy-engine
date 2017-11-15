#!/bin/bash
echo stop when error
set -e

echo skip pull request
if [ x"$TRAVIS_PULL_REQUEST" == "xfalse" ]; then
  echo set up tag
  if [ -n "$TRAVIS_TAG" ]; then
    export GIT_TAG=$TRAVIS_TAG
  else
    export GIT_TAG=$TRAVIS_BRANCH-${TRAVIS_COMMIT:0:7}
  fi

  echo clean stack build
  stack clean

  if [ -n "$IS_DOCKER" ]; then
    echo build docker image
    echo
    echo create folder
    cd $TRAVIS_BUILD_DIR
    mkdir -p docker.tmp/bin
    echo build fluffy  image
    cd $TRAVIS_BUILD_DIR
    stack install fluffy --ghc-options -O2 --ghc-options -threaded
    export GIT_TAG=`echo $GIT_TAG | sed 's/\//-/g'`
    export LATEST=latest
    export FLUFFY=fluffy
    echo copy files
    sudo cp $HOME/.local/bin/fluffy  docker.tmp/bin
    sudo cp $TRAVIS_BUILD_DIR/integration/dockerfiles/fluffy.dockerfile   docker.tmp
    cd docker.tmp
    docker build -t qinka/fluffy:$FLUFFY-$GIT_TAG   -f pb-auth.dockerfile              . || true
    docker tag      qinka/fluffy:$FLUFFY-$GIT_TAG   qinka/fluffy:$FLUFFY-$LATEST    || true
    echo push docker images
    docker push qinka/fluffy || true
  else
    echo skip building docker image
  fi
fi
