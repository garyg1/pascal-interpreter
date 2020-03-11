//fpc 3.0.0
program smoke;

// // asdf *) asdf
// // (* (* asdf *)
// (* // asdf *)
procedure asdf(x: integer; a: string);
    var y, t: integer;
    
begin
    writeln('Hey', x, y, a);
end;

function bsdf(x: integer; a: string): integer;
begin
    writeln('Hey', x, a);
    bsdf := 1;
end;

function loopo(): integer;
    var x: integer = 0;

begin
    writeln('loopo');
    while x < 5 do
    begin
        writeln(x);
        x := x + 2;
        loopo := -x;
    end;
end;

procedure loopp();
    var x : integer = 0;

begin
    writeln('loopp');
    while x < 5 do
    begin
        writeln(x);
        x := x + 2;
    end;
end;

var x : integer = 0;
const q : integer = 1;
begin
    writeln('Hello, world!, x is: ', x);
    // asdf(1, 'whoa');
    bsdf(1, 'asdf');

    if (false) then writeln('asdf')
    else writeln('bsdf');

    case (-7) of
        1, 2..3, 4: writeln('123');
        4, 5, -8..-6: writeln('456');
        else writeln('else');
    end;

    while x < 100 do
    begin
        writeln('break test');
        if x = 37 then break;
        x := 37;
    end;

    writeln(100 > 50.0);
    writeln(100 - 50.0 > 50.0);

    x := 0;
    while x < 100 do
    begin
        x := x + 10;
        if x < 70 then continue;
        writeln('continue test, x = ', x);
        x := x + 5;
    end;

    x := loopo();
    writeln(x);

    writeln(q, x, q);

    loopp();

    asdf(1, 'aaaaa');
end.
