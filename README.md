# Pascal Interpreter
Interpreter for a decent subset of Pascal, with full unit test coverage + system tests. (This was done for a class project.)

## Usage
#### Run Interpreter
```
cabal run RunPascal -- tests/SystemTests/<test-name>.pas
```

#### Run Tests (System + Unit)
```
cabal test
```

## Features Implemented
- many basic aspects of the language (if-then, while-do, for-do, break/continue, procedures/functions, static scoping)
- readln/writeln
- formal parameters, excluding pass-by-reference (i.e., `var`)

## Approach
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

## Main Dependencies
- happy (https://www.haskell.org/happy/)
- alex (https://www.haskell.org/alex/)

## Stats
```bash
$ find . | grep -E "(src|test).*\.hs" | xargs wc -l
     262 ./test/Pascal/StateSpec.hs
     883 ./test/Pascal/InterpretSpec.hs
      52 ./test/Pascal/ScopeSpec.hs
      23 ./test/Pascal/TestUtils.hs
      69 ./test/SystemSpec.hs
       0 ./test/Spec.hs
      98 ./src/Pascal/Data.hs
     203 ./src/Pascal/State.hs
      30 ./src/Pascal/Base.hs
      40 ./src/Pascal/Scope.hs
     349 ./src/Pascal/Interpret.hs
      12 ./src/Pascal/Wrapper.hs
      19 ./src/Pascal.hs
      20 ./src/Main.hs
    2060 total
```
