(* https://www.tutorialspoint.com/pascal/pascal_case_statement.htm *)
program case_test;
Var
   grade: integer;

begin
    grade := 1;

    (* 1 *)
    case (grade) of
      1 : Writeln(1);
      2, 3: Writeln(2);
      4 : Writeln(4);
    end;
      
    (* 2 *)
    grade := 2;
    case (grade) of
      1 : Writeln(1);
      2, 3: Writeln(2);
      4 : Writeln(4);
    end;

    (* 2 *)
    grade := 3;
    case (grade) of
      1 : Writeln(1);
      2, 3: Writeln(2);
      4 : Writeln(4);
    end;

    (* 4 *)
    grade := 4;
    case (grade) of
      1 : Writeln(1);
      2, 3: Writeln(2);
      4 : Writeln(4);
    end;

    grade := 5;
    case (grade) of
      1 : Writeln(1);
      2, 3: Writeln(2);
      4 : Writeln(4);
   end;    

  (* 3 *)
   case grade of
    1: Writeln(grade);
    else Writeln(3);
   end;

  (* 3 *)
  Case grade of
    1: Writeln(1);
    2: Writeln(2);
    else Writeln(grade * 5);
  end;

  grade := 1;
  case grade of
    1: case grade of 
          1: case grade of 
              1: Writeln(1);
              else Writeln(2);
            end;
          else Writeln(2);
      end;
    else Writeln(2);
  end;
end.