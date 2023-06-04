

/* 单线程测试，用于测试Typing */


if true then mutex<T1,T2> else mutex<T1,T3>;
/* mutex的join -- mutex<T1,T2> : mutex<T1> */

let l1 = mutex<T1,T2> in
let l2 = mutex<T1,T3> in
let t1 = ref<T1,T2> 0 in
let t2 = ref<T1,T3> 0 in
if true then t1 else t2;
/* ref的join -- <loc #0> : Ref<T1,T2,T3> Nat */

let a = mutex<T1,T2> in
  acquire a in !(ref<T1> 0);
/* 多加锁可以正常运行 */


let a = mutex<T1> in
  acquire a in 
    acquire a in 
      !(ref<T1> 0);
/* 会死锁，嵌套取同一个锁 */



let m = mutex<M> in
  let g = lambda<M> z:Ref<M> Nat. z := succ (!z) in
  let y1 = ref<M> 1 in 
  acquire m in g y1;
/* lambda可以声明需要的锁，内部访问时就不需要再获得该锁 */
let m = mutex<M> in
  let g = lambda z:Ref<M> Nat. z := succ (!z) in
  let y1 = ref<M> 1 in
  acquire m in g y1;
/* 不声明则会报错 */
let m = mutex<M> in
  let g = lambda<M> z:Ref<M> Nat. z := succ (!z) in
  let y1 = ref<M> 1 in g y1;
/* 只声明，application时没有加锁也会报错 */
let m = mutex<M> in
  let g = lambda<M> z:Ref<M> Nat. acquire m in z := succ (!z) in
  let y1 = ref<M> 1 in
  acquire m in g y1;
/* lambda声明外部需要，如果内部再次synchronize也会报错（死锁） */




let t = mutex<T1> in
  let f1 = lambda f:Unit <T1> -> Nat. 
    acquire t in f unit in
  let f2 = lambda<T1> _:Unit. 0 in
  f1 f2;
/* 正确 */

let t = mutex<T1> in
  let f1 = lambda f:Unit -> Nat. 
    acquire t in f unit in
  let f2 = lambda _:Unit. acquire t in 0 in
  f1 f2;
/* 报错，bind abstraction without explcit declaring forbidden mutex (which may cause dead mutex): T1 */

let t1 = mutex<T1> in
let t2 = mutex<T2> in
  let f1 = lambda f:Unit <T2> -> Nat. 
    acquire t2 in f unit in
  let f2 = lambda<T2> _:Unit. acquire t1 in 0 in
  f1 f2;
/* 报错，死锁 */

let x = mutex<X> in
let y = mutex<X> in
let buf1 = ref<X> 0 in
let flag = true in
  acquire if flag then x else y in buf1 := 1;
/* 正确，因为x和y都对应锁X */


let x = mutex<X> in
let y = mutex<Y> in
let buf1 = ref<X> 0 in
let flag = true in
  acquire if flag then x else y in 0;
/* 正确，因为没有访问内存，并且两个锁join依旧是锁 */


let x = mutex<X> in
let buf1 = ref<Z> 0 in
let flag = true in
  acquire x in !buf1;
/* 报错，因为锁Z没有被bind */

let x = mutex<X> in
let y = mutex<Y> in
let buf1 = ref<X> 0 in
let flag = true in
  acquire if flag then x else y in buf1 := 1;
/* 报错，因为mutex<X>和mutex<Y>的join为mutex<empty>，因此访问buf1的时候有可能没有拿到锁X */



/* ------------------------------------------------------------ */


/* 多线程测试，用于测试Evaluating */


x = mutex<X>;
y = mutex<Y>;

buf1 = ref<X> 0;
buf2 = ref<Y> 100;

checkParity = fix (lambda f:Nat->Bool->Bool. lambda n:Nat. lambda even:Bool.
  if iszero n then even
  else f (pred n) (if even then false else true));

isEven = lambda n:Nat. checkParity n true;


targetFunc = lambda _:Unit.
  if isEven tid then
    acquire x in
      acquire y in
        let tmp1 = succ (!buf1) in
        let _ = (buf1 := tmp1) in
        let tmp2 = pred (!buf2) in
        let _ = (buf2 := tmp2) in
          {res1=tmp1,res2=tmp2}
  else
    acquire x in
      acquire y in
        let tmp1 = pred (!buf2) in
        let _ = (buf2 := tmp1) in
        let tmp2 = succ (!buf1) in
        let _ = (buf1 := tmp2) in
          {res1=tmp2,res2=tmp1};
/* 这个函数正常工作 */

/*
targetFunc = lambda _:Unit.
  if isEven tid then
    acquire x in 
      let tmp1 = succ (!buf1) in
      let _ = (buf1 := tmp1) in
      acquire y in
        let tmp2 = pred (!buf2) in
        let _ = (buf2 := tmp2) in
          {res1=tmp1,res2=tmp2}
  else
    acquire y in
      let tmp1 = pred (!buf2) in
      let _ = (buf2 := tmp1) in
      acquire x in
        let tmp2 = succ (!buf1) in
        let _ = (buf1 := tmp2) in
          {res1=tmp2,res2=tmp1};
/* 这个函数会死锁 */
*/

/*
t = acquire x in fork { targetFunc unit };
/* 报错，持有锁的情况下fork */
*/

t1 = fork { targetFunc unit };
t2 = fork { targetFunc unit };
t3 = fork { targetFunc unit };
t4 = fork { targetFunc unit };
t5 = fork { targetFunc unit };
t6 = fork { targetFunc unit };
t7 = fork { targetFunc unit };
t8 = fork { targetFunc unit };
t9 = fork { targetFunc unit };
t10 = fork { targetFunc unit };

wait t1;
wait t2;
wait t3;
wait t4;
wait t5;
wait t6;
wait t7;
wait t8;
wait t9;
wait t10;

acquire x in !buf1;
acquire y in !buf2;
