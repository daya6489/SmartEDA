context("Weight of evidence")

test_that("test normal function", {
  X <- mtcars$gear
  Y <- mtcars$am
  expect_error(ExpWoeTable(X, Y))
  Y[4] <- NA
  expect_error(ExpWoeTable(X, Y))
})

test_that("test output object", {
  X <- mtcars$gear
  Y <- mtcars$am
  woe_val <- ExpWoeTable(X, Y, valueOfGood = 1)
str(woe_val)
  expect_output(str(woe_val), "3 obs")
  expect_output(str(woe_val), "11 variables")
  expect_equal(is.data.frame(woe_val), TRUE)
})
