program decl_test;


var a : double = 1;
var b : boolean = false;

var c : double = 1;
    c1, c2 : boolean;
    d : double = 2;
    e : boolean = true;

const f : double = 1;
    g : boolean = false;
    h = true;

begin
    Writeln(a);
    Writeln(b);
    Writeln(c);
    Writeln(c1, '|', c2); // should get default value
    Writeln(d);
    Writeln(e);
    Writeln(f);
    Writeln(g);
    Writeln(h);
end.