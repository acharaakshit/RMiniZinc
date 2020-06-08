test_that("model created successfully",{
  v1 = variable$new(type = "int", kind = "decision", name = "b")
  v2 = variable$new(type = "int", kind = "decision", name = "c")
  c1 = constraint$new(operator = "<=", LHS_expr = get_expression$new(variables = v1),
                      RHS_expression = get_expression$new(variables = v2))
  obj <- objective$new(type_of_problem = "satisfy")
  # decision variables have to be provided
  expect_error(model$new(constraints = c1, objective = objective_of_problem))
  # decision variables can't be provided in place of parameters
  expect_error(model$new(parameters = c(v1,v2), constraints = c1, objective = objective_of_problem))
})