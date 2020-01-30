context("summary statistics for individual categorical predictors")

test_that("test normal function", {
  X <- mtcars$gear
  Y <- mtcars$am
  expect_error(ExpStat(X, Y))
  Y[2] <- NA
  expect_error(ExpStat(X, Y))
})

test_that("test output object", {
  X <- mtcars$gear
  Y <- mtcars$am
  catstat <- ExpStat(X, Y, valueOfGood = 1)
  ctv <- chisq.test(X, Y)
  ctat <- as.numeric(round(ctv$statistic, 3))
  expect_output(str(catstat), "chr")
  expect_equal(ctat, as.numeric(catstat[2]))
})
?chisq.test
