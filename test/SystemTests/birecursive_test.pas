program birecursive_test;

function factorialB(n: integer): integer;
begin
    writeln('B called');
    if n = 0 then 
        factorialB := 1
    else begin
        factorialB := n * factorialA(n - 1);
    end; 
end;

function factorialA(n: integer): integer;
begin
    writeln('A called');
    if n = 0 then 
        factorialA := 1
    else begin
        factorialA := n * factorialB(n - 1);
    end;
end;

begin
    writeln(factorialA(6));
    writeln(factorialA(5));
    writeln(factorialB(6));
    writeln(factorialB(5));
end.