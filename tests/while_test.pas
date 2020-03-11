program while_test;

var x : integer = 1;
var did_break : boolean = false;
begin
    while x < 1000 do
    begin
        if x = 32 then
        begin
            x := x + 1;
            writeln('x = ', x, ', if was true');
            continue;
        end;

        x := x * 2;
        if x > 1000 then
            while x > 1050 do
            begin
                x := x - 1;
                writeln('while, ', x);
                if (x = 1052) then
                begin
                    did_break := true;
                    break;
                end;
            end;

        if did_break then
            break;

        writeln('x = ', x, ', didn''t break');
    end;
end.