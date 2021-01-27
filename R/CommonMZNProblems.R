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
  pVals = list(Int$new(n), Int$new(capacity), Array$new(intExpressions(profit))
               , Array$new(intExpressions(size)))
  names(pVals) = c("n", "capacity", "profit", "size")
  
  # set the missing parameters
  model = set_params(modData = pVals, model = model)
  
  solutions = mzn_eval(r_model = model)
  return(solutions$SOLUTIONS)
}


#' @title assignment problem
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
  return(solutions$SOLUTIONS)
}