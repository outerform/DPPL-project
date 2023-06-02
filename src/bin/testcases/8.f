x = mutex<X>;
y = mutex<Y>;

buf1 = ref<X> 0;
buf2 = ref<Y> 100;

targetFunc = lambda _:Unit.
     acquire x in
      acquire y in
      let tmp1 = succ (!buf1) in
      let _ = (buf1 := tmp1) in
        {res1=tmp1,res2=(! buf2)};


targetFunc2 = lambda _:Unit.
     acquire x in
      acquire y in
      let tmp2 = succ (!buf2) in
      let _ = (buf2 := tmp2) in
        {res1=(!buf1),res2=tmp2};

t1 = fork { targetFunc unit };
t2 = fork { targetFunc unit };
t3 = fork { targetFunc unit };
t4 = fork { targetFunc unit };
t5 = fork { targetFunc unit };
t6 = fork { targetFunc2 unit };
t7 = fork { targetFunc2 unit };
t8 = fork { targetFunc2 unit };
t9 = fork { targetFunc2 unit };
t10 = fork { targetFunc2 unit };

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