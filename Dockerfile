FROM ubuntu
MAINTAINER qinka
RUN add-apt-repository -y ppa:hvr/ghc
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ trusty-pgdg main" > /etc/apt/sources.list.d/pgdg.list
RUN wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -
RUN apt-get update
RUN apt-get -y install cabal-install-$CABALVER ghc-$GHCVER postgresql-9.4
RUN export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH
RUN cabal sandbox init
RUN cabal update
RUN cabal install
RUN ls -a
ADD .cabal-sandbox/YrarbilBackEnd/ybe.bin /usr/bin
EXPORT 3000
RUN echo '{"host":"localhost","dbname":"postgres","user":"qinka","password":"null","port":"2999"}' > sqlconfig.json
RUN mkdir /etc/yrarbilbackend
ADD sqlconfig.json /etc/yrarbilbackend
ENTRYPORT ybe.bin  /etc/yrarbilbackend
