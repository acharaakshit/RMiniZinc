int: nproducts;
set of int: Products = 1..nproducts;
array [Products] of int: profit;
array [Products] of string: pname;
int: nresources;
set of int: Resources = 1..nresources;
array [Resources] of int: capacity;
array [Resources] of string: rname;
array [Products, Resources] of int: consumption;
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
