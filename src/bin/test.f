x = mutex<A>;
y = mutex<B>;
c = ref<A>1;
t = acquire x in c := 2;
let x = acquire x in !c in 
x;