
let b = mutex<T1> in
let a = mutex<T2> in
  acquire a in 
  acquire b in !(ref<T1> 0);