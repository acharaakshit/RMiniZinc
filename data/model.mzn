var 1..100: bananas;

var 1..100: cakes;

constraint 250*bananas + 200*cakes <= 4000;

constraint 75*bananas + 150*cakes <= 2000;

constraint 100*bananas + 150*cakes <= 500;

constraint 75*cakes <= 500;

solve minimize 400*bananas + 450*cakes;

