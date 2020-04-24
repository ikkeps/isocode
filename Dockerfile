FROM alpine:edge

RUN apk update
RUN apk add ghc=8.8.3-r0 cabal wget

RUN apk add build-base

WORKDIR /opt/isocode

RUN cabal update

COPY . /opt/isocode

RUN cabal install --only-dependencies -j4

RUN cabal test
RUN cabal install exe:isocode --overwrite-policy=always --installdir=. --install-method=copy

CMD ["/opt/isocode/isocode"]