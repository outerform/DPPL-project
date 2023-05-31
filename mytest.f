/* Examples for testing */
t1 = fork{lambda x:Nat. succ x};
t2 = fork{0};
t3 = wait{t1};
t4 = wait{t2};
t3 t4;

r = ref<M> 1;
acquire mutex<M> !r;

