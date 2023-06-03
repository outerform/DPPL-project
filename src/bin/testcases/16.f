

f = let t = mutex<L> in let zz = 2 in lambda _:Unit. acquire t in zz;

let l = mutex<T> in
acquire l in f unit;

/*deadlock*/