program pass_by_ref_test;

procedure pure(inner3 : integer);
begin
    inner3 := 1000;
end;

procedure ref2(var inner2 : integer);
begin
    inner2 := 4;
    pure(inner2);
end;

procedure ref1(var inner : integer);
begin
    ref2(inner);
end;

function pure2(inner4 : integer): integer;
begin
    ref1(inner4);
    pure2 := inner4;
end;

var a : integer;
var b : integer;
begin
    a := 10;
    ref1(a);
    writeln(a); // 4

    a := 10;
    ref1(a + 5);
    writeln(a); // 10

    a := 10;
    ref1(((a)));
    writeln(a); // 4

    a := 10;
    ref1(((a)) + 1);
    writeln(a); // 10

    a := 10;
    b := pure2(a);
    writeln(a, '|', b); // 10|4
end.