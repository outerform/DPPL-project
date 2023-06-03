t = mutex<T1>;
f1 = lambda f:Unit[T1] -> Nat. 
    acquire t in f unit ;
f2 = lambda _:Unit. let x = acquire t in 0 in t;
  f1 f2;
/*acquire T1 twice in f1*/