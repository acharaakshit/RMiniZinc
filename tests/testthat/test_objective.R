test_that("objective set successfully",{
  # objective can only be of type satisfy, minimize or maximize
  expect_error(objective$new(type_of_problem = "max"))
  # expression should be provided for minimization or maximization problems
  expect_error(objective$new(type_of_problem = "maximize"))
  # expression provided can only be of class get_expression
  expect_error(objective$new(type_of_problem = "maximize", arithmetic_expression = "A+B"))
})