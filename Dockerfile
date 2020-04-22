FROM alpine:edge

RUN apk update
RUN apk add ghc=8.8.3-r0 cabal wget

RUN apk add build-base

WORKDIR /opt/isocode

RUN cabal update

# Hack so cabal does not check executable
RUN mkdir src ; touch src/Walk.hs
RUN mkdir test; touch test/Spec.hs
RUN touch README.md LICENSE

# Add just the .cabal file to capture dependencies
COPY ./*.cabal /opt/isocode/

RUN cabal install --only-dependencies -j4

COPY . /opt/isocode
RUN cabal configure

RUN cabal build
RUN cabal test

RUN strip dist/build/isocode/isocode

CMD ["/opt/isocode/dist/build/isocode/isocode"]