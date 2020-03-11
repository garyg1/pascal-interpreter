program const_reassignment_fails_test;

Const
    const1 = 2.0;

begin
    const1 := 3.0;
    (* throw ConstAssignmentException *)
end.