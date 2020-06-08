test_that("MiniZinc output parsed to R6 successfully",{
  # the argument can only be of type character
  expect_error(parsetoR6$new(a = 10))
})