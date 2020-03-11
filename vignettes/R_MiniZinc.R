## ----pressure, echo=FALSE, fig.cap="Structure of the model/functions to create the model", out.width = '60%'----
knitr::include_graphics("~/RMiniZinc/data/model_minizinc.png")

## ---- echo=FALSE, fig.cap="Obtaining the results", out.width = '50%'----------
knitr::include_graphics("~/RMiniZinc/data/get_solution.png")

## ---- echo=FALSE, fig.cap="Steps to check if the solution is correct", out.width = '30%'----
knitr::include_graphics("~/RMiniZinc/data/check_sol.png")

## ---- echo=FALSE--------------------------------------------------------------
path_of_model_mzn <- "~/RMiniZinc/data/model.mzn"

## -----------------------------------------------------------------------------
single_var_types <- "int"
single_var_names <- "n"
single_var_values <- c(5)
create_vars(single_var_names, single_var_types, data_file_bool = FALSE, 
            single_var_values= single_var_values,path = path_of_model_mzn)
single_dec_var_names <- c("wa","nt","sa","q","nsw","v","t")
single_dec_var_ranges <- list(c(1,3),c(1,3),c(1,3),c(1,3),c(1,3),c(1,3),c(1,3))
create_decision_vars(single_dec_var_names,single_dec_var_ranges, path = path_of_model_mzn)
constraint_list <- c("wa != nt","wa != sa","nt != sa","nt != q","sa != q","sa != nsw","sa != v","q != nsw","nsw != v")
setup_constraints(constraint_list, path = path_of_model_mzn)
type_of_problem <- 's' #satisfaction problem
setup_solvers(type_of_problem, path =  path_of_model_mzn)
default_output_bool <- TRUE
setup_output(path = path_of_model_mzn, default_output_bool = TRUE)


## ----results='markup'---------------------------------------------------------
 cat(get_results(path_of_model_mzn,solver = "Gecode", task = "solve",
                FlatZinc_bool = FALSE, data_file_bool = FALSE))

## ---- echo=FALSE--------------------------------------------------------------
path_of_model_mzc_mzn <- "~/RMiniZinc/data/model.mzc.mzn"

## -----------------------------------------------------------------------------
single_dec_var_types <- c("int","int","int","int","int","int","int")
setup_checker_model_input(single_dec_var_names,single_dec_var_types, constraint_list,
                          path = path_of_model_mzc_mzn)
cat(check_results(path_of_model_mzn = path_of_model_mzn,path_of_model_mzc_mzn = path_of_model_mzc_mzn))

## ----echo=FALSE---------------------------------------------------------------
path_of_data_dzn <- "~/RMiniZinc/data/data.dzn"

## -----------------------------------------------------------------------------
var1 <- "n"
val1 <- 5
create_data_dzn("n",5, path = path_of_data_dzn)

## -----------------------------------------------------------------------------
single_decision_var_names <- c("bananas","cakes")
single_decision_var_ranges <- list(c(1,100),c(1,100))
create_decision_vars(single_decision_var_names,single_decision_var_ranges, append_bool = FALSE,
                     path = path_of_model_mzn)
constraint_list <- c("250*bananas + 200*cakes <= 4000","75*bananas + 150*cakes <= 2000",
                     "100*bananas + 150*cakes <= 500","75*cakes <= 500")
setup_constraints(constraint_list, path = path_of_model_mzn)
type_of_problem <- "max" #maximization problem
arithmetic_expression <- "400*bananas + 450*cakes"
setup_solvers(type_of_problem, arithmetic_expression, path = path_of_model_mzn)
get_results(path_of_model_mzn,data_file_bool = FALSE)


## -----------------------------------------------------------------------------
single_decision_var_types <- c("int", "int")
setup_checker_model_input(single_decision_var_names,single_decision_var_types, constraint_list,
                          path = path_of_model_mzc_mzn)
cat(check_results(path_of_model_mzn = path_of_model_mzn,path_of_model_mzc_mzn = path_of_model_mzc_mzn))

## -----------------------------------------------------------------------------
single_decision_var_names <- c("bananas","cakes")
single_decision_var_ranges <- list(c(1,100),c(1,100))
create_decision_vars(single_decision_var_names,single_decision_var_ranges, append_bool = FALSE,
                     path = path_of_model_mzn)
constraint_list <- c("250*bananas + 200*cakes <= 4000","75*bananas + 150*cakes <= 2000",
                     "100*bananas + 150*cakes <= 500","75*cakes <= 500")
setup_constraints(constraint_list, path = path_of_model_mzn)
type_of_problem <- "min" #minimization problem
arithmetic_expression <- "400*bananas + 450*cakes"
setup_solvers(type_of_problem, arithmetic_expression, path = path_of_model_mzn)
get_results(path_of_model_mzn,data_file_bool = FALSE)


## -----------------------------------------------------------------------------
single_decision_var_types <- c("int", "int")
setup_checker_model_input(single_decision_var_names,single_decision_var_types, constraint_list,
                          path = path_of_model_mzc_mzn)
cat(check_results(path_of_model_mzn = path_of_model_mzn,path_of_model_mzc_mzn = path_of_model_mzc_mzn))

