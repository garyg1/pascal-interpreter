program precedence_test;

Var
    a : double = 3;
    b : double = 4;
    result : boolean;

Const
    t : boolean = true;
    f : boolean = false;
    s1 : string = 'alpha';
    s2 : string = 'Alpha';
    s3 : string = 'omega';
    s4 : string = 'alpha';
    s5 : string = 'alphabet';

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

    Writeln(
        s1 < s2,
        '|', s2 < s3,
        '|', s3 < s4,
        '|', s1 = s4,
        '|', s1 = s5,
    );
end.