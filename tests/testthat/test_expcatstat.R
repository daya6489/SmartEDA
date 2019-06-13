context("summary statistics for all character or categorical columns in the dataframe")

test_that("test normal function", {
  df <- iris
  df$Y <- 1
  expect_error(ExpCatStat(df))
  expect_error(ExpCatStat(c(1 : 10)))
  expect_error(ExpCatStat(df, Target = "Y"))
  expect_error(ExpCatStat(df, Target = "Species", result = NULL))
})

test_that("test output object", {
  options(warn = -1)
  catstat <- ExpCatStat(iris, Target = "Species")
  str(catstat)
  expect_output(str(catstat), "data.frame")
  expect_output(str(catstat), "4 obs")
})
