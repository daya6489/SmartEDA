context("Information value")

test_that("test normal function", {
  X <- mtcars$gear
  Y <- mtcars$am
  expect_error(ExpInfoValue(X, Y))
})

test_that("test output object", {
  X <- mtcars$gear
  Y <- mtcars$am
  ivvaue <- ExpInfoValue(X, Y, valueOfGood = 1)
  expect_output(str(ivvaue), "List of 2")
  expect_equal(ivvaue$`Information values`, 0.44)
})
