% taken from minizinc-examples https://github.com/MiniZinc/minizinc-examples

int: n = 3; % number of objects
set of int: OBJ = 1..n;
int: capacity = 9;
array[OBJ] of int: profit = [15,10,7];
array[OBJ] of int: size = [4,3,2];

array[OBJ] of var int: x; % how many of each object

constraint forall(i in OBJ)(x[i] >= 0);
constraint sum(i in OBJ)(size[i] * x[i]) <= capacity;
solve maximize sum(i in OBJ)(profit[i] * x[i]);

output ["x = ", show(x), "\n"];