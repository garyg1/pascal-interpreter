program recursive_procedure_test;


procedure a(n: integer);
begin
    writeln('a: ', n);
    if n > 0 then b(n - 1);
end;

procedure b(n: integer);
begin
    writeln('b: ', n);
    if n > 0 then writeln(c(n - 1));
end;

function c(n: integer): integer;
begin
    writeln('c: ', n);
    if n > 0 then a(n - 1);
    c := n - 1;
end;


var x : integer = 10;
begin
    a(x);
    writeln(x); // x shouldn't be modified
end.