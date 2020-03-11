program if_test;

begin
    (* true *)
    if true then
        Writeln(true)
    else Writeln(false);

    (* false *)
    if false then
        Writeln(true)
    else Writeln(false);

    (* 1 2 3 *)
    if true then
        begin
            Writeln(1);
            Writeln(2);
            Writeln(3);
        end
    else begin
        Writeln(4);
        Writeln(5);
        Writeln(6);
        end;

    (* 4 5 6 *)
    if ((( not not false ))) then
        begin
            Writeln(1);
            Writeln(2);
            Writeln(3);
        end
    else begin
        Writeln(4);
        Writeln(5);
        Writeln(6);
        end;
    
    (* true *)
    if 1 + 2 + 3 * (12 * 3) >= 100
    then
        Writeln(true)
    else
        Writeln(false);

    (* 2 3 *)
    if true then begin
        Writeln(2);
        Writeln(3);
    end;

    (* *)
    if false then begin
        Writeln(2);
        Writeln(3);
    end;

    (* assert that else if works *)

    (* third *)
    if false then Writeln('first')
    else if false then
    begin
        Writeln('second');
    end
    else if true then Writeln('third')
    else Writeln('fourth');

    (* fourth *)
    if false then Writeln('first')
    else if false then
    begin
        Writeln('second');
    end
    else if false then Writeln('third')
    else Writeln('fourth');

end.