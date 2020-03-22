program for_var_isconst;

var x : integer;
begin
    for x := 1 to 5 do
        // x is const so should fail
        for x := 2 to 5 do
            writeln(x);
end.