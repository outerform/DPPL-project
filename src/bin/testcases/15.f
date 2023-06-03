x = mutex<T1>;
a = ref <T1> 0;
acquire x in lambda<T1> _:Unit. !a;

let t = mutex<T> in
  let f = lambda _:Unit. acquire t in 0 in
  f;