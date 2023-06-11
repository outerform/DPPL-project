/* Examples for testing */
t1 = fork{lambda x:Nat. succ x};
t2 = fork{succ 0};
t3 = wait t1;
t4 = wait t2;
t3 t4;

buf = ref<X> 0;
acquire mutex<X> in !buf;
