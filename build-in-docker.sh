#!/bin/bash

docker build -t isocode .

CID=$(docker create isocode)
docker cp ${CID}:/opt/isocode/dist/build/isocode/isocode .
docker rm ${CID}