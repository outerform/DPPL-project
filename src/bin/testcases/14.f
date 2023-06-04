x = mutex<T1>;
a = ref <T1> 0;
acquire x in lambda _:Unit. !a;
/*should explcit annotate lock in abstraction*/
let t = mutex<T> in
  let f = lambda _:Unit. acquire t in 0 in
  f;

