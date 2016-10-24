
test_that("auc of valid input produces correct output", {
  expect_equal(auc(c(0,   0,   0.25, 0.5, 0.75, 1),
                   c(0,   0.5, 1,    1,   1,    1)),
               0.9375)
})

test_that("auc of invalid inputs produces appropriate errors", {
  expect_error(auc(c("A", "B", "C", "D", "E", "F"),
                   c(0,   0.5, 1,   1,   1,   1)),
               "roc::auc: invalid input:  A, B, C, D, E, F is not a numeric vector.")
  expect_error(auc(matrix(c(0,   0,   0.25, 0.5, 0.75, 1), nrow = 3),
                   c(0,   0.5, 1,    1,   1,    1)),
               "roc::auc: invalid input:  0, 0, 0.25, 0.5, 0.75, 1 is not a numeric vector.")
  expect_error(auc(c(0,   0,   1.25, 0.5, 0.75, 1),
                   c(0,   0.5, 1,    1,   1,    1)),
               "roc::auc: invalid input:  0, 0, 1.25, 0.5, 0.75, 1 contains invalid values.")
  expect_error(auc(c(0,   -1,  0.25, 0.5, 0.75, 1),
                   c(0,   0.5, 1,    1,   1,    1)),
               "roc::auc: invalid input:  0, -1, 0.25, 0.5, 0.75, 1 contains invalid values.")
  expect_error(auc(c(0,   0.5, 1,   1,   1,   1),
                   c("A", "B", "C", "D", "E", "F")),
               "roc::auc: invalid input:  A, B, C, D, E, F is not a numeric vector.")
  expect_error(auc(c(0,   0.5, 1,    1,   1,    1),
                   matrix(c(0,   0,   0.25, 0.5, 0.75, 1), nrow = 3)),
               "roc::auc: invalid input:  0, 0, 0.25, 0.5, 0.75, 1 is not a numeric vector.")
  expect_error(auc(c(0,   0.5, 1,    1,   1,    1),
                   c(0,   0,   1.25, 0.5, 0.75, 1)),
               "roc::auc: invalid input:  0, 0, 1.25, 0.5, 0.75, 1 contains invalid values.")
  expect_error(auc(c(0,   0.5, 1,    1,   1,    1),
                   c(0,   -1,  0.25, 0.5, 0.75, 1)),
               "roc::auc: invalid input:  0, -1, 0.25, 0.5, 0.75, 1 contains invalid values.")
  expect_error(auc(c(0,   0,   0.25, 0.5, 0.75),
                   c(0,   0.5, 1,    1,   1,    1)),
               "roc::auc: inputs are of unequal length.")
  expect_error(auc(c(0,   0,   0.25, 0.5, 0.75, 1),
                   c(0,   0.5, 1,    1,   1)),
               "roc::auc: inputs are of unequal length.")
})

