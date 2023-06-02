let x = mutex<X> in
let buf1 = ref<Z> 0 in
let flag = true in
  acquire x in !buf1;