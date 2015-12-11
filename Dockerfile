FROM index.docker.io/library/haskell:7.10.2
MAINTAINER qinka
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ trusty-pgdg main" > /etc/apt/sources.list.d/pgdg.list
RUN apt-get install wget
RUN wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
RUN apt-get update
RUN apt-get -y install postgresql-9.4
RUN cabal sandbox init
RUN cabal update
RUN cabal install -j9
RUN ls -a
ADD .cabal-sandbox/YrarbilBackEnd/ybe.bin /usr/bin
EXPORT 3000
RUN echo '{"host":"localhost","dbname":"postgres","user":"qinka","password":"null","port":"2999"}' > sqlconfig.json
RUN mkdir /etc/yrarbilbackend
ADD sqlconfig.json /etc/yrarbilbackend
ENTRYPORT ybe.bin  /etc/yrarbilbackend
