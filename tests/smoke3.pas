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

var a: integer = 10;
    x, y : integer;
    result: string;
    b : boolean;
    f : double;

const c = 1;
begin
    while fact(a) > 500 do begin
        a := a - 1;
    end;
    case a of
        1..3, 5: result := 'a was in 1..3 or 5';
        else result := '';
    end;

    writeln(1, 2, 3, 4);
    readln(x);
    readln(y);
    writeln(a, c, x, y, b, f, 'gary is cool', 'unnamed', 1.0);
end.