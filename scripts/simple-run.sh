#!/bin/bash

set -e

echo
echo Check
echo
if [ -z $(which docker) ]; then
    echo You need to install docker first!
    exit 1
else
    echo Find docker!
fi

if [ -n "$FLUFFY_DB_PASSWORD" ]; then
    echo Find env var FLUFFY_DB_PASSWORD, which will be use as password
else
    echo Can not find env var FLUFFY_DB_PASSWORD
    echo Use the default password fluffypassword as default
    export FLUFFY_DB_PASSWORD
fi

if [ -n "$FLUFFY_PREFIX" ]; then
    echo Find env var FLUFFY_PREFIX for naming containers as prefix of name: $FLUFFY_PREFIX-
else
    echo Can not find env var FLUFFY_PREFIX
    echo Use the prefix fluffy as default
    export FLUFFY_PREFIX=fluffy
fi

if [ -n "$FLUFFY_DB_PORT_E" ]; then
    echo Find env var FLUFFY_DB_PORT_E, the port will be exported
    export FLUFFY_DB_PORT_EF=" -p $FLUFFY_DB_PORT_E:$5432 "
fi

if [ -n "$FLUFFY_PORT" ]; then
    echo Find env var FLUFFY_PORT, the port will be used for back-end
else
    echo Can not find env var FLUFFY_PORT
    echo Use the port 3000 as default
    export FLUFFY_PORT=3000
fi

if [ -n "$FLUFFY_SPM_PATH" ]; then
    echo Find env var FLUFFY_SPM_PATH
else
    echo Can not find env var FLUFFY_SPM_PATH
    echo You need to point out the path where the question bank located
    echo For example, if one of those file path is
    echo     /Users/Qinka/Downloads/2017spm/XDExamReview2017/TrueFalse/Chapter_2_The_Project_Management_and_Information_Technology_Context.docx
    echo the env-var should be /Users/Qinka/Downloads/2017spm/XDExamReview2017
    exit 2
fi

if [ -n "$FLUFFY_SKIP_CHECK_MD5" ]; then
    echo Find env var FLUFFY_SKIP_CHECK_MD5 and skip check md5
    echo NOTE: Checking MD5 value is to make sure that data is right ones.
else
    export TT_PWD=`pwd`
    cd $FLUFFY_SPM_PATH
    curl -sSL https://raw.githubusercontent.com/Qinka/fluffy-engine/master/scripts/checksum | md5sum -c && echo Pass md5 check sum
    cd $TT_PWD
    export TT_PWD=''
fi



echo
echo docker pull the images
echo
docker pull qinka/fluffy:fluffy-latest && echo Pulled fluffy
docker pull postgres:latest && echo Pulled Postgres
docker pull haskell:latest && echo Pulled Haskell

echo
echo create images
echo


docker run -d --name $FLUFFY_PREFIX-db -e POSTGRES_PASSWORD=$FLUFFY_DB_PASSWORD postgres:latest && echo create postgres container
docker run -d --name $FLUFFY_PREFIX-be --link $FLUFFY_PREFIX-db:db -p $FLUFFY_PORT:$FLUFFY_PORT qinka/fluffy:fluffy-latest \
       -c "fluffy $FLUFFY_PORT host=db port=5432 user=postgres password=$FLUFFY_DB_PASSWORD" && echo create fluffy container
docker run -d -i --name $FLUFFY_PREFIX-haskell  --link fluffy-db:db  haskell:latest /bin/bash && echo create haskell container 

echo
echo  initialize database
echo

echo initialize tables
wget https://raw.githubusercontent.com/Qinka/fluffy-engine/master/database/fluffy.sql -O fluffy.sql
docker cp fluffy.sql $FLUFFY_PREFIX-db:/
rm fluffy.sql
docker exec $FLUFFY_PREFIX-db psql -d postgres -U postgres -f /fluffy.sql

echo import data
wget https://raw.githubusercontent.com/Qinka/fluffy-engine/master/scripts/update.hs -O update.hs
docker cp update.hs $FLUFFY_PREFIX-haskell:/
rm update.hs
docker exec $FLUFFY_PREFIX-haskell chmod a+x /update.hs
docker cp $FLUFFY_SPM_PATH  $FLUFFY_PREFIX-haskell:/
docker exec $FLUFFY_PREFIX-haskell git clone https://github.com/Qinka/fluffy-engine.git
docker exec $FLUFFY_PREFIX-haskell apt update
docker exec $FLUFFY_PREFIX-haskell apt install -y libpq5 libpg-dev
docker exec $FLUFFY_PREFIX-haskell cabal update
docker exec $FLUFFY_PREFIX-haskell cabal install --only-dependencies fluffy-engine/fluffy-parser
docker exec $FLUFFY_PREFIX-haskell cabal install postgresql-simple pandoc bytestring monad-logger fast-logger text
docker exec $FLUFFY_PREFIX-haskell cabal install fluffy-engine/fluffy-parser
docker exec $FLUFFY_PREFIX-haskell runhaskell /update.hs / host=db port=5432 user=postgres password=$FLUFFY_DB_PASSWORD
