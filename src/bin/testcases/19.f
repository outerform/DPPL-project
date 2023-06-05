l = mutex<T>;
r = ref<T> 10;
t = lambda f:Nat[T]->Unit. lambda x:Nat. if iszero x then unit 
else let _ = (acquire l in r := succ(!r)) in f (pred x);
fix t 10;


l = mutex<T>;
r = ref<T> 10;
t = lambda f:Nat[T]->Unit. lambda x:Nat. if iszero x then unit 
else acquire l in (let _ = (r := succ(!r)) in f (pred x));
fix t 10;