# fluffy-engine
A fluffy engine for XDU 2017 fall SS SPM final exam.

[![Build Status](https://travis-ci.org/Qinka/fluffy-engine.svg?branch=master)](https://travis-ci.org/Qinka/fluffy-engine)
[![license](https://img.shields.io/github/license/qinka/fluffy-engine.svg)](https://github.com/Qinka/fluffy-engine/blob/master/LICENSE)
[![Docker image](https://img.shields.io/badge/docker-image-orange.svg)](https://hub.docker.com/r/qinka/fluffy/)
[![ImageLayers Layers](https://img.shields.io/imagelayers/layers/qinka/fluffy/fluffy-latest.svg)](https://hub.docker.com/r/qinka/fluffy/)
[![ImageLayers Size](https://img.shields.io/imagelayers/image-size/qinka/fluffy/fluffy-latest.svg)](https://hub.docker.com/r/qinka/fluffy/)

## Usage

### Get Quick Start

You can run the following command to deploy it directly, but you might need read *[Env](#Env)* and *[Dependence](#Dependence)* first:
```bash
curl -sSL http://raw.githubusercontent.com/Qinka/fluffy-engine/master/scripts/simple-run.sh | sh
```
Now you can go to have a tea, and take a rest, because that will take a lots of time.
If you setting back-end port to `3000`, you can then visit `http://localhost:3000` on your browser.

#### Dependence

You need install:

* Docker

* md5sum

* curl

* wget

* bash or bash-like shell

#### Env

##### `FLUFFY_SPM_PATH`
When you try to run that script, you need set environment variables.
One you must set is `FLUFFY_SPM_PATH`. This env var sets up the path of question banks of course(SPM).
For example, if your bank located at `/home/qinka/2017spm/XDExamReview2017`,
or say one file of bank is `/home/qinka/2017spm/XDExamReview2017/TrueFalse/Chapter_2_The_Project_Management_and_Information_Technology_Context.docx`,
you should set `FLUFFY_SPM_PATH` up as `/home/qinka/2017spm/XDExamReview2017/TrueFalse`.

##### `FLUFFY_SKIP_CHECK_MD5`
Moreover, the script will check the MD5 of those files to make sure that process of parsing will work.
If you want to skip this step, you need to set environment variable `FLUFFY_SKIP_CHECK_MD5` up as any non-empty string.

##### `FLUFFY_DB_PASSWORD`

This env var will set up your database's password, and the default is `fluffypassword`

##### `FLUFFY_PREFIX`

This env var will set up a prefix for each container's name, and the default is `fluffy`

##### `FLUFFY_DB_PORT_E`

This env var will set up a export port for database, and the default is "not export", or say empty.

##### `FLUFFY_PORT`

This env var will set up the port of back-end, and `3000` is default.

#### Tips

You might need to use a docker mirror and hackage mirror site.
For the former, you need search it on the Internet.
For the latter, you need edit the cabal configure file in the container to change to mirror site, (you can search it on the Internet).

### Run

#### First step is pull the images.

The images include:

* `qinka/fluffy:fluffy-latest`
* `postgtrs:latest`
* `haskell:latest`

You can pull them from Docker Hub or other mirror sites with commoands:

```bash
docker pull qinka/fluffy:fluffy-latest
docker pull postgres:latest
docker pull haskell:latest
```

The image `fluffy:fluffy-latest` is the back-end.
It need a PostgreSQL database(image `postgres`).
The image `haskell` is used to run the scripts to upload data from `*.docx`s.

#### Second step is run the images.

You can run it via docker-compose, or just run the command one-by-one.
The key idea is fluffy need to be linked with database.

The following is the example running with shell command one-by-one.

Create and run the PostgreSQL image: `postgres:latest` :
```bash
docker run -d --name fluffy-db -e POSTGRES_PASSWORD=fluffypassword postgres:latest
```
The detail can be found on [Docker Hub - postgres](https://hub.docker.com/_/postgres/).

Create and run the Fluffy image: `qinka/fluffy:fluffy-latest`:
```bash
docker run -d --name fluffy-be --link fluffy-db:db -p 3000:3000 fluffy:fluffy-latest -c "fluffy 3000 host=db port=5432 user=postgres password=fluffypassword"
```

Then you can visit host-of-docker:3000, and the home page will be shown.

*(optional for haskell image)*

To update the data to database with [script](scripts/update.hs), you also need
a container with haskell's image.
So you can run:
```bash
docker run -d --name fluffy-haskell --link fluffy-db:db haskell:latest
```

#### Third step is import the data into database with script.

You need firstly initialize the sql table to PostgreSQL database.
Copy the sql file to `postgres` image:
```bash
wget https://raw.githubusercontent.com/Qinka/fluffy-engine/master/database/fluffy.sql 
docker cp fluffy.sql fluffy-db:/fluffy.sql
rm fluffy.sql
```
After sql file copied, run the `psql` to initialize database with [sql script](database/fluffy.sql):
```bash
docker exec fluffy-db psql -d postgres -U postgres -f /fluffy.sql
```

Till now, the database is initialized. Next thing to do is import the data into database
with the [script](scripts/update.hs).
Copy the script into database:
```bash
wget -sSL https://raw.githubusercontent.com/Qinka/fluffy-engine/master/scripts/update.sh
docker cp - fluffy-haskell:/update.hs
rm update.hs
docker exec fluffy-haskell chmod a+x /update.hs
```
Copy the SPM's question bank into container `fluffy-haskell`:
```bash
docker cp /path/of/spm/s/question/bank fluffy-haskell:/
```
Than  run the script:
```bash
docker cp /path/to/question/bank/Fillintheblanks fluffy-haskell:/
docker cp /path/to/question/bank/MultipleChoice  fluffy-haskell:/
docker cp /path/to/question/bank/TrueFalse       fluffy-haskell:/
docker cp /path/to/question/bank/WritingPart     fluffy-haskell:/
docker exec fluffy-haskell git clone https://github.com/Qinka/fluffy-engine.git
docker exec fluffy-haskell apt update
docker exec fluffy-haskell apt install -y libpq5 libpg-dev
docker exec fluffy-haskell cabal update
docker exec fluffy-haskell cabal install --only-dependencies fluffy-engine/fluffy-parser
docker exec fluffy-haskell cabal install postgresql-simple pandoc bytestring monad-logger fast-logger text
docker exec fluffy-haskell cabal install fluffy-engine/fluffy-parser
docker exec fluffy-haskell runhaskell /update.hs / host=db port=5432 user=postgres password=fluffypassword
```

#### Final

Then you can visit the site to check the data.

### Customization

The fluffy has a way to customize: there is a javascript file, named `prelude.js`, which will be loaded. You can change this javascript file to customize this site.
The all the static file can be placed or added into the data directory of fluffy(in docker image is `/usr/share/fluffy`).
So when customizing fluffy, you can add the files into data directory of fluffy and those file can be found under route `/static`.

#### With Dockerfile

You can add the file you want to add into fluffy via creating a new docker with dockerfile, for example:
```Dockerfile
FROM qinka/fluffy:fluffy-latest
MAINTAINER Johann Lee <me@qinka.pro>
ADD /your/host/files /usr/share/fluffy
```

#### With `docker cp`

You can also copy the files by commoand:
```
docker cp /path/to/your/files fluffy:/usr/share/fluffy
```
