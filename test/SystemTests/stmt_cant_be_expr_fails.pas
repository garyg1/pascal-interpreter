program stmt_cant_be_expr;

const a = 1;
begin
    a = 2; // this is an expr, not an assign
    // should fail on parse
end.