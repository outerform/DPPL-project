

f = let t = mutex<T> in let zz = 2 in lambda _:Unit. acquire t in zz;

let l = mutex<L> in
acquire l in f unit;

/*right*/