program smoke3;

function fact(n : integer): integer;
begin
    if n <= 1 then fact := 1
    else fact := fact(n - 1) * n;
end;

    var a: integer;
    var b : double;
begin
    readln(a);
    writeln(fact(a));
end.