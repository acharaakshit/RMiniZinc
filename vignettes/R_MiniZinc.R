## ---- echo=FALSE--------------------------------------------------------------
library(rminizinc)
model <- tempfile(fileext=".mzn" )

## -----------------------------------------------------------------------------
par1_type <- "int"
par1_name <- "n"
par1_value <- 5
new_param(model = model,par_name =  par1_name,par_type = par1_type, single_par_val = par1_value)
single_dec_var_names <- c("wa","nt","sa","q","nsw","v","t")
single_dec_var_ranges <- list(c(1,3),c(1,3),c(1,3),c(1,3),c(1,3),c(1,3),c(1,3))
create_decision_vars(model = model,single_dec_var_names,single_dec_var_ranges)
constraint_list <- c("wa != nt","wa != sa","nt != sa","nt != q","sa != q","sa != nsw","sa != v","q != nsw","nsw != v")
setup_constraints(model=model, constraint_list)
type_of_problem <- 's' #satisfaction problem
specify_problem(model = model, type_of_problem)

## ----results='markup'---------------------------------------------------------
 str_options = solver_options(statistics = TRUE)
 cat(get_mzn_results(model = model,str_options = str_options))

## ---- echo=FALSE, results='hide'----------------------------------------------
 file.remove(model)

