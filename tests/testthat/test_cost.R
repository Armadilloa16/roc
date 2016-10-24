
test_that("cost of valid input produces correct output", {
  expect_equal(cost(c(0,   0,   0.25, 0.5, 0.75, 1),
                   c(0,   0.5, 1,    1,   1,    1)),
               c(0.5, 0.25, 0.125, 0.25, 0.375, 0.5))
  expect_equal(cost(c(0,   0,   0.25, 0.5, 0.75, 1),
                    c(0,   0.5, 1,    1,   1,    1),
                    3),
               c(0.25, 0.125, 0.1875, 0.375, 0.5625, 0.75))
})

test_that("cost of invalid inputs produces appropriate errors", {
  expect_error(cost(c("A", "B", "C", "D", "E", "F"),
                   c(0,   0.5, 1,   1,   1,   1)),
               "roc::cost: invalid input:  A, B, C, D, E, F is not a numeric vector.")
  expect_error(cost(matrix(c(0,   0,   0.25, 0.5, 0.75, 1), nrow = 3),
                   c(0,   0.5, 1,    1,   1,    1)),
               "roc::cost: invalid input:  0, 0, 0.25, 0.5, 0.75, 1 is not a numeric vector.")
  expect_error(cost(c(0,   0,   1.25, 0.5, 0.75, 1),
                   c(0,   0.5, 1,    1,   1,    1)),
               "roc::cost: invalid input:  0, 0, 1.25, 0.5, 0.75, 1 contains invalid values.")
  expect_error(cost(c(0,   -1,  0.25, 0.5, 0.75, 1),
                   c(0,   0.5, 1,    1,   1,    1)),
               "roc::cost: invalid input:  0, -1, 0.25, 0.5, 0.75, 1 contains invalid values.")
  expect_error(cost(c(0,   0.5, 1,   1,   1,   1),
                   c("A", "B", "C", "D", "E", "F")),
               "roc::cost: invalid input:  A, B, C, D, E, F is not a numeric vector.")
  expect_error(cost(c(0,   0.5, 1,    1,   1,    1),
                   matrix(c(0,   0,   0.25, 0.5, 0.75, 1), nrow = 3)),
               "roc::cost: invalid input:  0, 0, 0.25, 0.5, 0.75, 1 is not a numeric vector.")
  expect_error(cost(c(0,   0.5, 1,    1,   1,    1),
                   c(0,   0,   1.25, 0.5, 0.75, 1)),
               "roc::cost: invalid input:  0, 0, 1.25, 0.5, 0.75, 1 contains invalid values.")
  expect_error(cost(c(0,   0.5, 1,    1,   1,    1),
                   c(0,   -1,  0.25, 0.5, 0.75, 1)),
               "roc::cost: invalid input:  0, -1, 0.25, 0.5, 0.75, 1 contains invalid values.")
  expect_error(cost(c(0,   0,   0.25, 0.5, 0.75),
                   c(0,   0.5, 1,    1,   1,    1)),
               "roc::cost: inputs are of unequal length.")
  expect_error(cost(c(0,   0,   0.25, 0.5, 0.75, 1),
                   c(0,   0.5, 1,    1,   1)),
               "roc::cost: inputs are of unequal length.")
})

