int: nproducts = 2;
set of int: Products = 1..nproducts;
array [Products] of int: profit = [400, 500];
array [Products] of string: pname = ["banana-cake", "chocolate-cake"];
int: nresources = 5;
set of int: Resources = 1..nresources;
array [Resources] of int: capacity = [4000, 6, 2000, 500, 500];
array [Resources] of string: rname = ["flour", "banana", "sugar", "butter", 
    "cocoa"];
array [Products, Resources] of int: consumption = [| 250, 2, 75, 100, 0 | 
                                                     200, 0, 150, 150, 75 |];
constraint assert(forall ( r in Resources, p in Products ) ( 
    consumption[p, r]>=0 
), "Error: negative consumption");
int: mproducts = max ( p in Products ) ( 
                     min ( r in Resources where consumption[p, r] > 0 ) ( 
                         capacity[r] div consumption[p, r] 
                     ) 
                 );
array [Products] of var 0..mproducts: produce;
array [Resources] of var 0..max(capacity): used;
constraint forall ( r in Resources ) ( 
    used[r]==sum ( p in Products ) ( consumption[p, r]*produce[p] ) /\ 
    used[r]<=capacity[r] 
);
solve maximize sum ( p in Products ) ( profit[p]*produce[p] );
output [ ((show(pname[p])++" = ")++show(produce[p]))++";\n" | p in Products ]++
[ ((show(rname[r])++" = ")++show(used[r]))++";\n" | r in Resources ];
