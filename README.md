# Pascal Interpreter
Interpreter for a decent subset of Pascal, with full unit test coverage + system tests. (This was done for a class project.)

## Usage
#### Run Interpreter
```
cabal run RunPascal -- <file-name>.pas
```

#### Run Tests (System + Unit)
```
cabal test
```

## Features Implemented
- many basic aspects of the language (if-then, while-do, for-do, break/continue, procedures/functions, static scoping)
- readln/writeln
- formal parameters, excluding pass-by-reference (i.e., `var`)

## Pascal Examples
#### Recursive Factorial
```pascal
program recursive_test;

function factorial(n: integer): integer;
begin
    if n = 0 then 
        factorial := 1
    else
        factorial := n * factorial(n - 1);
end;

begin
    writeln(factorial(6));
    writeln(factorial(5) * factorial(10));
end.
```
#### Fast Modular Exponentiation (FME)
```pascal
program fme;

// fast modular exponentiation
// adapted from wikipedia
function fme(base, exp, modulus : integer): integer;
var i : integer;
    result : integer = 1;
begin
    while exp > 0 do
    begin
        if (exp mod 2) = 1 then
            result := (result * base) mod modulus;
        exp := exp / 2;
        base := (base * base) mod modulus;
    end;
    fme := result;
end;

begin
    writeln(fme(2, 10, 500));
    writeln(fme(3, 10, 500));
    writeln(fme(3, 1000, 500));
end.
```

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
