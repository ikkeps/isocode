#!/bin/bash

docker build -t isocode .

CID=$(docker create isocode)
docker cp ${CID}:/opt/isocode/isocode .
docker rm ${CID}