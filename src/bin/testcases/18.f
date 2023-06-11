l = mutex<T>;
r = ref<T> 10;
t = lambda f:Nat->Unit. lambda x:Nat. if iszero x then unit 
else acquire l in r := succ(!r);
fix t 10;