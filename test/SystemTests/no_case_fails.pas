program no_case_fails;

const x = 1;
begin
    // should fail on parse
    case x of
        else writeln('asdf');
    end;
end.