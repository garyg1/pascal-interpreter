program smoke3;

function fact(n : integer): integer;
begin
    if n <= 1 then fact := 1
    else fact := n * fact(n - 1);
end;

var a: integer;
begin
    a := fact(4);
end.