test_that("results can be obtained successfully",{
  # a valid model object should be given to the constructuor
  expect_error(results$new(model = 10, result_type = "R6", all_solutions = TRUE))
  v1 =  variable$new(kind = "decision", type = "int", domain = c(0,4))
  c1 =  constraint$new(operator = "<", 
                 LHS_expression = get_expression$new(variables = v1), 
                 RHS_expression = get_expression$new(variables = 2))
  obj = objective$new(type_of_problem = "satisfy")
  mod = model$new(decisions = c(v1), constraints = c(c1), objective = obj)
  # only string and R6 output can be obtained
  expect_error(results$new(model = mod, result_type = "json", all_solutions = TRUE))
})