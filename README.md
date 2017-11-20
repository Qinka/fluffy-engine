# fluffy-engine
A fluffy engine for XDU 2017 fall SS SPM final exam.

[![Build Status](https://travis-ci.org/Qinka/fluffy-engine.svg?branch=master)](https://travis-ci.org/Qinka/fluffy-engine)
[![license](https://img.shields.io/github/license/qinka/fluffy-engine.svg)](https://github.com/Qinka/fluffy-engine/blob/master/LICENSE)
[![Docker image](https://img.shields.io/badge/docker-image-orange.svg)](https://hub.docker.com/r/qinka/fluffy/)
[![ImageLayers Layers](https://img.shields.io/imagelayers/layers/qinka/fluffy/fluffy-latest.svg)](https://hub.docker.com/r/qinka/fluffy/)
[![ImageLayers Size](https://img.shields.io/imagelayers/image-size/qinka/fluffy/fluffy-latest.svg)](https://hub.docker.com/r/qinka/fluffy/)

## Usage (Undone, TODO)

### Get Quick Start

TODO (with simple-run.sh <- TODO)

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
docker run -d --name fluffy-db -e POSTGRES_PASSWORD=fluffypassword postgres
```
The detail can be found on [Docker Hub - postgres](https://hub.docker.com/_/postgres/).

Create and run the Fluffy image: `qinka/fluffy:fluffy-latest`:
```bash
docker run -d -name fluffy-be --link fluffy-db:db -p 3000:3000 fluffy -c "fluffy 3000 host=db port=5432 user=postgres password=fluffypassword"
```

Then you can visit host-of-docker:3000, and the home page will be shown.

*(optional for haskell image)*

To update the data to database with [script](scripts/update.hs), you also need
a container with haskell's image.
So you can run:
```bash
docker run -d --name fluffy-haskell --link fluffy-db:db
```

#### Third step is import the data into database with script.

You need firstly initialize the sql table to PostgreSQL database.
Run :
```bash
docker exec -it fluffy-db /bin/bash
```
and then you will connect with container which host database.
After connected with that database, run the `psql` to connect with the database
and initialize it with [sql script](database/fluffy.sql):
```bash
TODO
```

#### Final

TODO

### Custom with Docker file

TODO

### Custom via add file to container

TODO
