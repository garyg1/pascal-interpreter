program fme;

// fast modular exponentiation
// adapted from wikipedia
function fme(base, exp, modulus : integer): integer;
var i : integer;
var result : integer = 1;
begin
    while exp > 0 do
    begin
        if (exp mod 2) = 1 then
            result := (result * base) mod modulus;
        exp := exp / 2;
        base := (base * base) mod modulus;
    end;
    fme := result;
end;

begin
    writeln(fme(2, 10, 500));
    writeln(fme(3, 10, 500));
    writeln(fme(3, 1000, 500));
end.
