program pass_by_ref_test;

// TODO fix "var"
function refPass(x, y: integer; var a, b, c : integer): integer;
begin
    a := x;
    b := y;
    c := 4;
    refPass := -1;
end;

var x : integer = 1;
    y : integer = 2;
    globalA : integer;
    globalB : integer;
    globalC : integer;
    retVal : integer;

begin
    retVal := refPass(x, y, globalA, globalB, globalC);
    writeln(x, y, globalA, globalB, globalC, retVal);
end.