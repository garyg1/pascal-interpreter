program scope;

var i : integer = 0;
var j : integer = 0;
var x: string = 'global-x';
var y: string = 'global-y';

function getx2(): string;
var
    x: string = 'getx2-x';
begin
    getx2 := x;
    writeln(x);
    writeln(y);
end;

function getx(): string;
var
    x: string = 'getx-x';

begin
    writeln(x);
    writeln(y);
    getx := getx2();
end;

// should be able to modify global i in function scope
procedure reassignGlobal();
begin
    i := 5;
end;

// global i should not be changed
procedure reassignLocal();
var i : integer;
begin
    reassignGlobal();
    i := 4;
end;

begin
    writeln(getx());
    writeln(y);

    reassignLocal();

    // should be able to access and modify
    // i even in a bunch of nested loop scopes
    while i > 0 do
        while i > 0 do
            while i > 0 do
                for j := 1 to 5 do
                    if i > 0 then
                    begin
                        i := i - 1;
                        writeln('i was not zero but ', i);
                    end;
    writeln('i is now ', i);
end.