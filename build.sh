echo Building Resources...
rcc -binary marble.qrc -o marble.rcc
echo Building Haskell...
ghc -O2 --make marble.hs
