program precedence_test;

Var
    a : double = 3;
    b : double = 4;
    result : boolean;

Const
    t : boolean = true;
    f : boolean = false;

begin
    (* assert all operators work as expected *)
    result := a = b;
    Writeln(result);

    result := a <> b;
    Writeln(result);

    result := a > b;
    Writeln(result);

    result := a < b;
    Writeln(result);

    a := 4;
    result := a <= b;
    Writeln(result);

    result := a >= b;
    Writeln(result);

    (* does comparison play nice with boolean operators? *)

    result := (t = f) and (t <> f);
    Writeln(result);
end.