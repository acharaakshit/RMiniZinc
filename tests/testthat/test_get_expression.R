test_that("expression created successfully",{
  # only valid arithmetic operators can be used
  expect_error(get_expression$new(variables = c(10,20), arithmetic_operator = "="))
  # the number of arithmetic operators used should be valid
  expect_error(get_expression$new(variables = c(10,20), arithmetic_operator = "+,+"))
})