let x = mutex<X> in
let y = mutex<Y> in
let buf1 = ref<X> 0 in
let flag = true in
  acquire if flag then x else y in 0;