int: n = 5;
int: capacity = 200;
array [1..n] of int: profit = [1300, 1000, 520, 480, 325];
array [1..n] of int: size = [90, 72, 43, 40, 33];
array [1..n] of var bool: x;
constraint sum ( i in 1..n ) ( size[i]*bool2int(x[i]) )<=capacity;
solve maximize sum ( i in 1..n ) ( profit[i]*bool2int(x[i]) );
output ["x = ", show(x), "\n"];
