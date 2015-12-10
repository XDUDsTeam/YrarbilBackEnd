FROM ubuntu
MAINTAINER qinka
ADD .cabal-sandbox/YrarbilBackEnd/ybe.bin /usr/bin
EXPORT 3000
RUN echo '{"host":"localhost","dbname":"postgres","user":"qinka","password":"null","port":"2999"}' | ybe.bin
