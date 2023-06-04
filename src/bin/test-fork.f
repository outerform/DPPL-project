
lambda x:Nat. succ x;
let t1 = fork{lambda x:Nat. succ x} in
let t2 = fork{succ 0} in 
let t3 = wait t1 in
let t4 = wait t2 in
t3 t4;

p1 = fork{succ 0};
pp = let p2 = fork{lambda x:Nat. succ x} in wait p2;
pp;
p2 = fork{lambda x:Nat. succ x};
wait p2;

x = mutex<A>;
c = ref<A>1;
t = acquire x in c := 2;
let x = acquire x in !c in x;

wait p2;
wait p1;
