t = mutex<T1>;
f2 = lambda _:Unit. let x = acquire t in 0 in x;
f2 unit;
/* right */