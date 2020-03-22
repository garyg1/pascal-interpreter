program f;

var x : integer;
begin
    // test to
    for x := 13 to 15 do
        writeln('to: ', x);
    writeln('to yields: ', x);

    // test downto
    for x := 15 downto 13 do
        writeln('downto: ', x);
    writeln('downto yields: ', x);

    // test break
    for x := 10 to 14 do
    begin
        writeln('to: ', x);
        if x = 13 then break;
    end;

    writeln('break at: ', x);

    // test continue
    for x := 10 to 14 do
    begin
        if x = 12 then continue;
        writeln('to: ', x);
    end;

    writeln('continue yields: ', x);
end.