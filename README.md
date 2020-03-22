## Pascal Interpreter

### Contact
```
Gary Gurlaskie
UFID: 6761-4364
COP 4020 Spring 2020, University of Florida
Prof. Alin Dobra
```

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
     781    3571   32171 ./test/Pascal/InterpretSpec.hs
      52     269    2222 ./test/Pascal/ScopeSpec.hs
      15      64     414 ./test/Pascal/TestUtils.hs
      69     253    2376 ./test/SystemSpec.hs
       0       6      43 ./test/Spec.hs
      98     325    2225 ./src/Pascal/Data.hs
     201     931    6221 ./src/Pascal/State.hs
      30     126     816 ./src/Pascal/Base.hs
      40     187    1244 ./src/Pascal/Scope.hs
     341    1829   13454 ./src/Pascal/Interpret.hs
      12      34     502 ./src/Pascal/Wrapper.hs
      19      36     518 ./src/Pascal.hs
      20      65     512 ./src/Main.hs
    1899    8564   70864 total
```