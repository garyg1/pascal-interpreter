program precedence_test;

Var
    result : double;
    bool_result : boolean;

begin
    (* test * and + *)
    result := 3 + 2 * 4;
    Writeln(result); (* 11 *)

    result := (3 + 2) * 4;
    Writeln(result); (* 20 *)

    (* test * and - *)
    result := 3 - 2 * 4;
    Writeln(result); (* -5 *)

    result := (3 - 2) * 4;
    Writeln(result); (* 4 *)

    (* test / and + *)
    result := 3 + 2 / 4;
    Writeln(result); (* 3 *)

    result := (3 + 2) / 4;
    Writeln(result); (* 1.0 *)

    (* test * and / *)
    result := 4 / 2 / 2;
    Writeln(result); (* 1.0 *)

    result := 4 / 2 * 2;
    Writeln(result); (* 4.0 *)

    result := 4 * 2 / 2;
    Writeln(result); (* 4.0 *)

    (* test + and - *)
    result := 10 - 9 + 8;
    Writeln(result); (* 9.0 *)

    result := 10 - (9 + 8);
    Writeln(result); (* -7.0 *)

    result := 10 + -9;
    Writeln(result); (* 1.0 *)

    (* test && over || *)
    bool_result := true or true and false;
    Writeln(bool_result); (* true *)

    bool_result := (true OR true) AND false;
    Writeln(bool_result); (* false *)

    (* test && over ! *)
    bool_result := not false AND false;
    Writeln(bool_result); (* false *)

    // mod has higher precedence
    writeln(4 + 3 mod 4); // 7
    writeln(4 * 3 mod 4); // 0
    writeln(4 - 3 mod 4); // 1

end.