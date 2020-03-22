program writeln_literals_test;

var a : double;
var b : boolean;

const c = false;
const d = 4;

begin
    a := 5;
    b := true;

    Writeln(a);
    Writeln(b);
    Writeln(c);
    Writeln(d);
    Writeln('i am a string \literal');
    Writeln('escape sequence is double quote '' '' ');
    Writeln('');
    WRITELn();
end.