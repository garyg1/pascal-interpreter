program case_insensitive_test;

var a1b2c3 : double = 1;

begin
    writeln(a1b2c3);
    writeln(A1B2C3);
    writeln(a1B2c3);
    
    (* asssigment lookup should be case-insensitive *)
    A1B2C3 := 2;

    writeln(a1b2c3);
    writeln(A1B2C3);
    writeln(a1B2c3);
end.