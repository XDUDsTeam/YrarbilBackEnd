FROM index.docker.io/library/haskell:7.10.2
MAINTAINER qinka
ADD . /src
RUN ls -a /src
RUN cd /src && cabal sandbox init
RUN apt-get update
RUN apt-get -y install wget
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ trusty-pgdg main" > /etc/apt/sources.list.d/pgdg.list
RUN wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
RUN apt-get update
RUN apt-get -y install postgresql-9.4
RUN cabal update
RUN bash /src/Dockerfile.sh
EXPORT 3000
CMD ybe.bin /etc/yrarbilbackend/sqlconfig.json