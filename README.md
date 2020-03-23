## Pascal Interpreter

### Contact
```
Gary Gurlaskie
UFID: 6761-4364
COP 4020 Spring 2020, University of Florida
Prof. Alin Dobra
```

### Features Implemented
- all basic requirements (while-do, for-do, break/continue, procedures/functions, static scoping)
- readln/writeln
- formal parameters, except pass-by-reference (i.e., `var`)
- used monads (see below)

### Approach

#### Monads
I chose to implement the interpreter using a monad stack, including `State`, `Reader`, and `Except`. The stack is defined in `src/Pascal/State.hs`.
```haskell
type AppState = ExceptT Events (StateT PState (ReaderT AppIO IO))
type AppIO = (InputStream ByteString, OutputStream ByteString)
type AppReturn a = (Either Events a, PState)

runApp :: AppIO -> AppState a -> IO (AppReturn a)
runApp appIO fn = runReaderT (runStateT (runExceptT fn) new) appIO
```

##### Why?
- State needs to be passed around everywhere.
- Readln or Writeln can be called nearly anywhere, so access is needed to an I/O object.
- `continue` and `break` throw and catch exceptions, but state still must be passed.

#### Dependency Injection
Instead of using the IO monad directly, I wrapped stdin/stdout using the `io-streams` library. I made the istream/ostream accessible with the `Reader` monad. 

##### Why?
- Made it possible to test I/O within Haskell/HSpec without dependending on the filesystem
- Can inject an in-memory output stream using the `knob` library.

### Dependencies
- happy (https://www.haskell.org/happy/)
- alex (https://www.haskell.org/alex/)
- see the `.cabal` file for list of all libraries used

### Usage
#### Installation
```
cabal install
```

#### Running
```
cabal run RunPascal -- tests/<test-name>.pas
```

### Stats
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