program smoke3;

function basecase(): integer;
begin
    basecase := 1;
end;


function fact(n : integer): integer;
var d: integer = n - 1;
begin
    if n <= 1 then fact := basecase()
    else fact := n * fact(d);
end;

procedure temp(n : integer);
begin
    a := fact(n);
end;

var a: integer;
begin
    temp(5);
end.