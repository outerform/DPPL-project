let t = mutex<T1> in
  let f1 = lambda f:Unit -> Nat. 
    acquire t in f unit in
  let f2 = lambda _:Unit. acquire t in 0 in
  f1 f2;
/* type of f2 mismatch (Unit->Nat) */