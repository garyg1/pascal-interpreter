program comment_test;

begin
    (* *) Writeln('should print this'); (* *)
    (* (* Writeln('shouldn''t print this'); *) *)
    // asdf I am a comment (* * * this shlud be fine)
    (* // lkjasdfilasdf *) writeln('not comment (* no way son *)') (* asdf *); // writeln('(* i am comment *)');
end.