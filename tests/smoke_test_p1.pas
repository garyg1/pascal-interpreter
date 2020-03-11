program smoke_test;

Var
    _yacc123_ : double;
    woke : double = 5.0;
    i : double = 0.123412341234;
    b : boolean;
    e : integer;

Const
    a : double = 1.0;
    c = false;

begin
    _yacc123_ := 11.1 + -2 * 5;
    _yacc123_ := _yacc123_ * woke;
    
    (* _yacc123_ = 1337; *)
    Writeln(_yacc123_);
    Writeln(woke);

    Readln(i);

    Writeln('math functions');
    Writeln(sqrt(i (* i am a comment! *)));
    Writeln(sin(i));
    Writeln(cos(i));
    Writeln(exp(i));
    Writeln(ln(i));

    Readln(i);

    writeln('logical statements');
    if (i > 10.0)
    then Writeln(i)
    else Writeln(a);

    e := trunc(i);

    if (a = 1) or not not (a = 1)
    then case e of
        1: Writeln(i * 100);
        2: WrItElN(i * 1000);
        else Writeln(a);
    end
    else Writeln(-1);
end.