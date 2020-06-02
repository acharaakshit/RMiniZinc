test_that("constraints can be set successfully",{
  var1 = variable$new(type = "float", kind = "decision")
  var2 = variable$new(type = "float", kind = "decision")
  LHS_expr = get_expression$new(variables = c(var1,var2), arithmetic_operator = "+" )
  RHS_expr = get_expression$new(variables = c(var1,var2), arithmetic_operator = "*" )
  # can't use any other operator than logical operators
  expect_error(constraint$new(operator = "->", LHS_expression = LHS_expr, RHS_expression = RHS_expr))
  # both LHS and RHS expressions should be provided
  expect_error(constraint$new(operator = ">=", LHS_expression = LHS_expr))
  # expressions can only be objects of class get_expression
  expect_error(constraint$new(operator = ">=", LHS_expression = LHS_expr, RHS_expression = "var1*var2"))
  
})