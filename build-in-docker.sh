#!/bin/bash

docker build -t isocode .

CID=$(docker create isocode)
docker cp ${CID}:/bin/isocode .
docker rm ${CID}