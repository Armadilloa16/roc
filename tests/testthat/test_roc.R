
test_that("roc of valid input produces correct output", {
  expect_equal(roc(c(1,     2,     3,     4,     4,     5),
                   c(FALSE, FALSE, FALSE, TRUE,  FALSE, TRUE)),
               data.frame(TPR = c(0,   0.5, 1,    1,   1,    1),
                          FPR = c(0,   0,   0.25, 0.5, 0.75, 1),
                          t   = c(Inf, 5,   4,    3,   2,    1)))
})

test_that("roc of invalid inputs produces appropriate errors", {
  expect_error(roc(c("A",   "B",   "C",   "D",   "E",   "F"),
                   c(FALSE, FALSE, FALSE, TRUE,  FALSE, TRUE)),
               "roc::roc: invalid input:  A, B, C, D, E, F is not a numeric vector.")
  expect_error(roc(matrix(c(1,     2,     3,     4,     4,     5), nrow = 3),
                   c(FALSE, FALSE, FALSE, TRUE,  FALSE, TRUE)),
               "roc::roc: invalid input:  1, 2, 3, 4, 4, 5 is not a numeric vector.")
  expect_error(roc(c(1,     2,     3,     4,     4,     5),
                   c("A",   "B",   "C",   "D",   "E",   "F")),
               "roc::roc: invalid input:  A, B, C, D, E, F is not a boolean vector.")
  expect_error(roc(c(1,     2,     3,     4,     4,     5),
                   matrix(c(FALSE, FALSE, FALSE, TRUE,  FALSE, TRUE), nrow = 3)),
               "roc::roc: invalid input:  FALSE, FALSE, FALSE, TRUE, FALSE, TRUE is not a boolean vector.")
  expect_error(roc(c(1,     2,     3,     4,     4),
                   c(FALSE, FALSE, FALSE, TRUE,  FALSE, TRUE)),
               "roc::roc: inputs are of unequal length.")
  expect_error(roc(c(1,     2,     3,     4,     4,     5),
                   c(FALSE, FALSE, FALSE, TRUE,  FALSE)),
               "roc::roc: inputs are of unequal length.")
})

test_that("roc of empty or NA inputs produces appropriate warnings", {
  expect_warning(roc(vector(mode="numeric", length=0),
                     vector(mode="logical", length=0)),
                 "roc::roc: empty inputs.")
  expect_warning(roc(NA_real_, TRUE),
                 "roc::roc: all entries of input are NA.")
  expect_warning(roc(0, NA),
                 "roc::roc: all entries of input are NA.")
  expect_warning(roc(c(1,     NA,     3,     4,     4,     5),
                     c(FALSE, FALSE, FALSE, TRUE,  FALSE, TRUE)),
                 "roc::roc: 1 invalid entries removed.")
  expect_warning(roc(c(1,     2,     3,     4,     4,     5),
                     c(FALSE, NA,    FALSE, TRUE,  FALSE, TRUE)),
                 "roc::roc: 1 invalid entries removed.")
  expect_warning(roc(c(1,     NA,     3,     NA,     4,     5),
                     c(FALSE, FALSE, FALSE, TRUE,  FALSE, NA)),
                 "roc::roc: 3 invalid entries removed.")
})
