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

    for a := 1 to 10 do begin
        if a = 5 then break;
        writeln(a);
    end;
    writeln(a);
    for a := 10 downto 1 do 
    begin
        writeln(a);
    end;

    writeln(ln(1), exp(1), cos(1), sin(1), sqrt(1));
end.