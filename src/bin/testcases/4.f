let a = mutex<T1> in
  acquire a in 
    acquire a in 
      !(ref<T1> 0);
/*nested acquire one lock*/