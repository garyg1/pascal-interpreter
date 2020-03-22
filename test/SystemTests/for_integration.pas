program for_integration;

procedure a(x, y : integer);
begin
    writeln('procedure. x: ', x, ', y: ', y);
    x := x + 1;
    y := y + 1;
end;

var x, y, z : integer;
begin
    for x := 1 to 3 do
    begin
        for y := 4 to 6 do
        begin
            a(x, y);
            writeln('loop-inner. x: ', x, ', y: ', y);
            
            // z should not be const though
            z := y;
            while z > 4 do
            begin
                z := z - 1;
                writeln('z: ', z);
                continue;
                writeln('should not be reached');
            end;
            if x = 2 then break;
        end;
        writeln('loop-outer. x: ', x, ', y: ', y);
    end;

    
    writeln('x: ', x, ', y: ', y);
end.