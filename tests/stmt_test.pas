program stmt_test;

// which things are allowed as statements?

procedure proc1();
begin
    writeln('procedures are fine');
end;


function func1(): string;
begin
    writeln('functions are fine');
    func1 := 'yes indeed';
end;


var i : integer;
    b : boolean;
begin
    writeln('this is fine');
    
    proc1();    
    func1();

    ((func1())); // nested function calls are fine
    ((proc1())); // nested procedure calls are fine
    
    b := i = 0;
    writeln('b = ', b);
    
    if b then writeln('ifs are fine');

    case i of
        0: writeln('cases are fine');
    end;
    
    for i := 1 to 1 do
        writeln('fors are fine');
    
    while (true) do
    begin
        writeln('whiles are fine');
        break; // keywords are cool as well
    end;
end. 