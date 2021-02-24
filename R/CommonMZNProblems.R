# API functions for common constraint programming problems
# These examples are taken from https://github.com/MiniZinc/minizinc-examples.

#' @title knapsack problem
#' @description 
#' Solve a simple knapsack problem (Goal is to maximize the profit)
#' @param n number of items
#' @param capacity total capacity of carrying weight
#' @param profit profit corresponding to each item
#' @param size weight/size of each item
#' @export
knapsack = function(n, capacity, profit, size){
  knapsack_string = 
  "
  int: n; % number of objects
  set of int: OBJ = 1..n;
  int: capacity;
  array[OBJ] of int: profit;
  array[OBJ] of int: size;

  array[OBJ] of var int: x; % how many of each object

  constraint forall(i in OBJ)(x[i] >= 0);
  constraint sum(i in OBJ)(size[i] * x[i]) <= capacity;
  solve maximize sum(i in OBJ)(profit[i] * x[i]);
  "
  model = suppressMessages(mzn_parse(model_string = knapsack_string))
  
  # list of the data
  pVals = list(Int$new(n), Int$new(capacity), Array$new(intExpressions(profit), 
                                                        dimranges = list(IntSetVal$new(1, n)))
               , Array$new(intExpressions(size), 
                           dimranges = list(IntSetVal$new(1, n))))
  names(pVals) = c("n", "capacity", "profit", "size")
  
  # set the missing parameters
  model = set_params(modData = pVals, model = model)
  
  solutions = mzn_eval(r_model = model)
  return(list(model = model, solution = solutions))
}


#' @title assignment problem 2
#' @description 
#' Solve an assignment problem (Goal is to minimize the cost)
#' @param n number of agents
#' @param m number of tasks
#' @param cost m x n 2D array where each row corresponds to the cost of each task for that agent.
#' (to be provided as 1-D vector)
#' @export

assignment = function(n, m, cost){
  assignment_string = 
    '
    int: n;
    set of int: DOM = 1..n;
    int: m;
    set of int: COD = 1..m;
    array[DOM,COD] of int: cost;
    
    array[DOM] of var COD: task;
    
    include "alldifferent.mzn";
    constraint alldifferent(task);
    
    
    solve minimize sum(w in DOM)
                (cost[w,task[w]]);
    '
  model = mzn_parse(model_string = assignment_string)
  
  # list of the data
  pVals = list(Int$new(n), Int$new(m), 
               Array$new(exprVec = intExpressions(cost),
                                                 dimranges = list(IntSetVal$new(1,n), IntSetVal$new(1,m))))
  names(pVals) = c("n", "m", "cost")
  
  # set the missing parameters
  model = set_params(modData = pVals, model = model)
  
  solutions = mzn_eval(r_model = model)
  return(list(model = model, solution = solutions))
}

#' @title assignment problem 2
#' @description 
#' Solve an assignment problem 
#' Winston "Operations Research", page 398, swimming team example
#' Model created by Hakan Kjellerstrand(hakank(at)bonetmail.com)
#' See : http://www.hakank.org/minizinc/assignment2.mzn
#' @param rows number of columns
#' @param cols number of tasks
#' @param cost cost matrix (to be provided as 1-D vector)
#' @export

assignment_2 = function(rows, cols, cost){
  model_string = 
    "
      predicate assignment(array[int, int] of var 0..1: x, 
                     array[int, int] of int: cost, 
                     var int: s
                 ) = 
          forall(i in index_set_1of2(x)) (sum(j in index_set_2of2(x)) (x[i,j]) = 1) /\\
          if card(index_set_1of2(x)) = card(index_set_2of2(x)) then
             forall(j in index_set_2of2(x)) (sum(i in index_set_1of2(x)) (x[i,j]) = 1)
          else  
             forall(j in index_set_2of2(x)) (sum(i in index_set_1of2(x)) (x[i,j]) <= 1)
          endif
          /\\  
          s = sum(i in index_set_1of2(x), j in index_set_2of2(x)) (x[i,j]*cost[i,j])
        ;

      int: rows;
      int: cols;
      array[1..rows, 1..cols] of var 0..1: x;
      array[1..rows, 1..cols] of int: cost;
      var int: total_sum;
      
      solve minimize total_sum;
      
      constraint 
                 assignment(x, cost, total_sum)
                %  /\\ total_sum <= 181
      ;
    "
  model = mzn_parse(model_string = model_string)
  
  # list of the data
  pVals = list(Int$new(rows), Int$new(cols), 
               Array$new(exprVec = intExpressions(cost),
                         dimranges = list(IntSetVal$new(1,rows), IntSetVal$new(1,cols))))
  
  names(pVals) = c("rows", "cols", "cost")
  
  # set the missing parameters
  model = set_params(modData = pVals, model = model)
  
  solutions = mzn_eval(r_model = model)
  return(list(model = model, solution = solutions))
}


#' @title magic squares problem
#' @description 
#' Solve a magic squares problem in MiniZinc
#' Model created by Hakan Kjellerstrand(hakank(at)bonetmail.com)
#' See : http://www.hakank.org/minizinc/magic_square.mzn
#' @param n order of magic square
#' @export

magic_square = function(n){
  model_string = 
    '
    include "globals.mzn";

    int: n; 
    
    int: total = ( n * (n*n + 1)) div 2;
    % var 0..n*n*n: total;
    array[1..n,1..n] of var 1..n*n: magic;
    
    
    % solve satisfy;
    solve :: int_search(
            [magic[i,j] | i in 1..n, j in 1..n], 
            first_fail,
            indomain_random, % indomain_median,
            complete) 
        satisfy;


    constraint
      all_different([magic[i,j] | i in 1..n, j in 1..n]) % :: bounds % domain
      /\\
      forall(k in 1..n) (
         sum(i in 1..n) (magic[k,i]) = total /\\
         sum(i in 1..n) (magic[i,k]) = total
      )
      /\\ % diagonal
      sum(i in 1..n) (magic[i,i]) = total
      /\\ % diagonal
       sum(i in 1..n) (magic[i,n-i+1]) = total
    
      /\\ total = ( n * (n*n + 1)) div 2
    
    ;

    '
  model = mzn_parse(model_string = model_string)
  
  # list of the data
  pVals = list(Int$new(n))
  
  names(pVals) = c("n")
  
  # set the missing parameters
  model = set_params(modData = pVals, model = model)
  
  solutions = mzn_eval(r_model = model)
  return(list(model = model, solution = solutions))
}


#' @title magic series problem
#' @description 
#' Solve a magic series problem in MiniZinc
#' Model created by Hakan Kjellerstrand(hakank(at)bonetmail.com)
#' See : http://www.hakank.org/minizinc/magic_series.mzn
#' @param n order of magic square
#' @export

magic_series = function(n){
  model_string = 
    '
    include "globals.mzn"; 

    int: n;
    int: n2 = n*n;
    int: m = n*(n2+1);
    
    % decision variables
    array[1..n] of var 1..n2: x;
    
    % solve satisfy;
    solve :: int_search(x, first_fail, indomain_min, complete) satisfy;
    
    constraint
      sum(x)*2 = m /\\
      all_different(x) /\\
      increasing(x) 
    ;


    '
  model = mzn_parse(model_string = model_string)
  
  # list of the data
  pVals = list(Int$new(n))
  
  names(pVals) = c("n")
  
  # set the missing parameters
  model = set_params(modData = pVals, model = model)
  
  solutions = mzn_eval(r_model = model)
  return(list(model = model, solution = solutions))
}

#' @title production planning problem
#' @description 
#' simple production planning problem taken from
#' https://github.com/MiniZinc/minizinc-examples
#' Goal is to maximize the profit
#' @param nproducts number of different products
#' @param profit profit for each product (1-D vector)
#' @param pnames names of each product (1-D vector)
#' @param nresources number of resources
#' @param capacity amount of each resource available (1-D vector)
#' @param rnames names of each resource (1-D vector)
#' @param consumption units of each resource required to produce 
#' 1 unit of product (2-D vector to be provided as 1-D vector)
#' @export

production_planning = function(nproducts, profit, pnames, nresources,
                               capacity, rnames, consumption){
  model_string = 
    '
    % Number of different products
    int: nproducts; 
    set of int: Products = 1..nproducts;  
    
    %profit per unit for each product
    array[Products] of int: profit;
    array[Products] of string: pname; 
    
    %Number of resources
    int: nresources; 
    set of int: Resources = 1..nresources; 
    
    %amount of each resource available
    array[Resources] of int: capacity; 
    array[Resources] of string: rname;

    %units of each resource required to produce 1 unit of product
    array[Products, Resources] of int: consumption; 
    constraint assert(forall (r in Resources, p in Products) 
               (consumption[p,r] >= 0), "Error: negative consumption");
    
    % bound on number of Products
    int: mproducts = max (p in Products) 
                         (min (r in Resources where consumption[p,r] > 0) 
                              (capacity[r] div consumption[p,r]));
    
    % Variables: how much should we make of each product
    array[Products] of var 0..mproducts: produce;
    array[Resources] of var 0..max(capacity): used;
    
    % Production cannot use more than the available Resources:
    constraint forall (r in Resources) (     
          used[r] = sum (p in Products)(consumption[p, r] * produce[p]) 
          /\\ used[r] <= capacity[r]
    );    

    % Maximize profit
    solve maximize sum (p in Products) (profit[p]*produce[p]);
    '
  model = mzn_parse(model_string = model_string)
  
  if(length(profit) != nproducts || length(pnames) != nproducts){
    stop("length of profit and/or pnames must be equal to nproducts")
  }
  
  if(length(capacity) != nresources || length(rnames) != nresources){
    stop("length of capacity and/or names must be equal to nresources")
  }
  
  if(length(consumption) != nproducts*nresources){
    stop("length of consumption must be equal to nproducts*nresources")
  }
  
  # list of the data
  pVals = list(Int$new(nproducts),
               Array$new(exprVec = intExpressions(profit), dimranges = list(IntSetVal$new(1, nproducts))),
               Array$new(exprVec = stringExpressions(pnames), list(IntSetVal$new(1, nproducts))),
               Int$new(nresources),
               Array$new(exprVec = intExpressions(capacity), dimranges = list(IntSetVal$new(1, nresources))),
               Array$new(exprVec = stringExpressions(rnames), list(IntSetVal$new(1, nresources))),
               Array$new(exprVec = intExpressions(consumption), dimranges = list(IntSetVal$new(1, nproducts),
                                                                              IntSetVal$new(1, nresources))))
  
  names(pVals) = c("nproducts", "profit", "pname", "nresources",
                   "capacity", "rname", "consumption")
  
  # set the missing parameters
  model = set_params(modData = pVals, model = model)
  
  solutions = mzn_eval(r_model = model)
  return(list(model = model, solution = solutions))
}