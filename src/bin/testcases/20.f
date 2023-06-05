l = mutex<T>;
r = ref<T> 10;
t = lambda f:Nat[T]->Unit.  acquire l in lambda x:Nat. ;
fix t 10;