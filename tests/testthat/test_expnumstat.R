context("Summary statistics for numerical variables")

test_that("test normal function", {
  expect_error(ExpNumStat(mtcars, by = "ABC", gp = "gear",
                          Qnt = c(0.1, 0.2), MesofShape = 2))
  expect_error(ExpNumStat(mtcars, by = "G", gp = NULL,
                          Qnt = c(0.1, 0.2), MesofShape = 2))
  expect_error(ExpNumStat(iris$Species))
})

test_that("test output object", {
  numstat <- ExpNumStat(mtcars, by = "A")
  expect_output(str(numstat), "data.frame")
})
