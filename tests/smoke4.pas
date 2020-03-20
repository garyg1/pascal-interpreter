program smoke4;

var x : integer = 3;
begin
    while x > 0 do begin
        writeln('x was ', x);
        if x = 1 then break;
        x := x - 1;
        
    end;
    writeln('x was ', x);
end.