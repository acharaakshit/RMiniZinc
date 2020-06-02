test_that("varaibles can be declared and initialized successfully",{
          par_int <- variable$new(kind = "parameter", type = "int", value = 10)
          expect_null(par_int$expr)
          # decision variables can't take values
          expect_error(variable$new(kind = "decision", type = "int", value = 10))
          })
