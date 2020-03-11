int: bananas; 

int: cakes; 

output [
 if 
250*bananas + 200*cakes <= 4000 /\ 
75*bananas + 150*cakes <= 2000 /\ 
100*bananas + 150*cakes <= 500 /\ 
100*bananas + 150*cakes <= 500
 then "CORRECT" 
 else "INCORRECT" 
 endif ];   
