let t = mutex<T1> in
  let f1 = lambda f:Unit <T1> -> Nat. 
    acquire t in f unit in
  let f2 = lambda<T1> _:Unit. 0 in
  f1 f2;
/* right */