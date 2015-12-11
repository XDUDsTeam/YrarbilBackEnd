pwd
ls
cd src
pwd
ls
cabal help
cabal help sandbox
cabal help install
cabal sandbox init
cabal install -j9
cd .cabal-sandbox/bin/
ls -a
cp ybe.bin /usr/bin
mkdir /etc/YrarbilBackend
echo '{"host":"localhost","dbname":"postgres","user":"qinka","password":"null","port":"2999"}' > /etc/YrarbilBackend/sqlconfig.json