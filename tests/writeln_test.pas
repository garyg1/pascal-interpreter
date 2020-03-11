program writeln_test;

Var
    bool_var : boolean;
    double_var : double;
    int_var : integer;
    string_var : string;

Const
    bool_const = false;
    double_const = 3.0;
    int_const = 5;
    string_const = 'i am a teapot';

begin
    double_var := -2.0;
    bool_var := true;
    int_var := -4;
    string_var := 'i am a coffee pot';
    Writeln(double_const,
        '|', bool_const,
        '|', int_const,
        '|', string_const,
        '|', double_var,
        '|', bool_var,
        '|', int_var,
        '|', string_var
    );
end.