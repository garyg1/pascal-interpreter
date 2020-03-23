program marshal_test;

function sum_of_squares(n: integer): integer;
var i : integer;
    sum : integer = 0;
begin
    for i := 1 to n do begin
        sum_of_squares := sum_of_squares + (i * i);
    end;
end;

function xor_n_times(n: integer): boolean;
var i : integer;
begin
    for i := 1 to n do begin
        xor_n_times := xor_n_times xor true;
    end;
end;

begin
    writeln(sum_of_squares(10));
    writeln(xor_n_times(11));
end.