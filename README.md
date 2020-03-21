## Pascal Interpreter

### Dependencies
- happy (https://www.haskell.org/happy/)
- alex (https://www.haskell.org/alex/)

### Usage
#### Installation
```
cabal install
```

#### Running
```
cabal run RunPascal -- tests/<test-name>.pas
```

#### Stats
```bash
$ find . | grep -E "(src|test).*\.hs" | xargs wc
     221     868    8146 ./test/Pascal/StateSpec.hs
     775    3542   31963 ./test/Pascal/InterpretSpec.hs
      52     269    2222 ./test/Pascal/ScopeSpec.hs
      14      58     347 ./test/Pascal/TestUtils.hs
       0       6      43 ./test/Spec.hs
      98     325    2225 ./src/Pascal/Data.hs
     188     858    5590 ./src/Pascal/State.hs
      30     126     816 ./src/Pascal/Base.hs
      40     187    1244 ./src/Pascal/Scope.hs
     331    1780   13093 ./src/Pascal/Interpret.hs
      12      34     502 ./src/Pascal/Wrapper.hs
      17      31     464 ./src/Pascal.hs
      19      63     473 ./src/Main.hs
    1797    8147   67128 total
```