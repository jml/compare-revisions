# compare-revisions

[![Circle CI](https://circleci.com/gh/jml/compare-revisions/tree/master.svg?style=shield)](https://circleci.com/gh/jml/compare-revisions/tree/master)

Show how sets of images differ, by revision

Contains:

* compare-revisions-api -- API definition for compare-revisions
* compare-revisions-server -- Server implementation of the compare-revisions API

## What it is

## Why you might want it

## How to use it

### Natively

Build and install the code with `stack install` and then run with:

    compare-revisions --port 8080

This will start a server that you can reach at http://localhost:8080/

### With Docker

Create a Docker image with:

    make image

The last line of successful `make` output will be the name of the image, e.g.
`compare-revisions:master-1a2b3cd`.

You can then run the image like so:

    docker run -p 8080:80 compare-revisions:master-1a2b3cd --port 80

And you can reach the server at http://localhost:8080/ if you are running
Docker natively. If you're on a Mac and
using [Docker Machine](https://docs.docker.com/machine/), you can run:

    open http://$(docker-machine ip):8080/

To browse to the running server.
