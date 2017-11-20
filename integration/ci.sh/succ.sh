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
    stack install cabal-install alex happy

    if [ -n "$IS_DOCKER" ]; then
        echo build docker image
        echo

        echo create folder
        cd $TRAVIS_BUILD_DIR
        mkdir -p docker.tmp/root

        echo build fluffy  image
        cd $TRAVIS_BUILD_DIR
        echo build fluffy
        cd fluffy
        echo cabal update
        cabal update
        #echo install dependencies
        #cabal install --only-dependencies
        echo configure
        export 
        cabal configure --prefix='/usr' --datasubdir='fluffy' \
          --package-db=clear --package-db=`stack path --local-pkg-db` \
          --package-db=`stack path --global-pkg-db`  \
          --package-db=`stack path --snapshot-pkg-db` \
          --enable-optimization=2 --ghc-options="-thread"
        echo build
        cabal build
        echo copy
        cabal copy --destdir=$TRAVIS_BUILD_DIR/docker.tmp/root -v
        cd $TRAVIS_BUILD_DIR

        export GIT_TAG=`echo $GIT_TAG | sed 's/\//-/g'`
        export LATEST=latest
        export FLUFFY=fluffy
        echo copy files
        sudo cp $TRAVIS_BUILD_DIR/integration/dockerfiles/fluffy.dockerfile   docker.tmp

        cd docker.tmp
        docker build -t qinka/fluffy:$FLUFFY-$GIT_TAG   -f fluffy.dockerfile          . || true
        docker tag      qinka/fluffy:$FLUFFY-$GIT_TAG   qinka/fluffy:$FLUFFY-$LATEST    || true
        echo push docker images
        docker push qinka/fluffy || true
    else
        echo skip building docker image
    fi
fi
