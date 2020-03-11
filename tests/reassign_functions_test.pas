program reassign_functions_test;

procedure hasJob(name, job : string);
begin
    writeln(name, ' has job ', job);
end;

procedure hasCar(name, car : string);
begin
    writeln(name, ' has car ', car);
end;

function hasLife(name: string; life : boolean): boolean;
begin
    if life then writeln(name, ' has a life')
    else writeln(name, ' has no life');
    hasLife := life;
end;


begin
    hasJob('gary', 'programmer');
    hasCar('gary', 'toyota');
    hasJob := hasCar;
    hasJob('gary', 'programmer');
    hasCar('gary', 'toyota');

    // reassign procedure to function
    hasLife('gary', false);
    hasCar := hasLife;
    hasCar('gary', true);

    // reassign function to procedure
    hasLife('gary', false);
    hasLife := hasJob; // := hasCar
    hasLife('gary', 'toyota');
end.