program boolean_smoke_test;

Const f : boolean = false;
Const t : boolean = true;

begin
    Writeln(not f and not f);
    Writeln(not t or not t);
    Writeln(not not not f);
    Writeln(f or f and t or t);
    Writeln(f xor not f);
    Writeln(t xor not t);
end.