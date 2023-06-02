
let b = mutex<T1> in
let a = mutex<T2> in
  acquire b in 
  acquire a in !(ref<T1> 0);