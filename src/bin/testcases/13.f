let t1 = mutex<T1> in
let t2 = mutex<T2> in
  let f1 = lambda f:Unit <T2> -> Nat. 
    acquire t2 in f unit in
  let f2 = lambda<T2> _:Unit. acquire t1 in 0 in
  f2;

/*may cause dead lock*/