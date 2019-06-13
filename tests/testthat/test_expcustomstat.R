context("Customized summary statistics")

test_that("test normal function", {
  df <- iris
  xyz <- df$Sepal.Length
  expect_error(ExpCustomStat(xyz, Cvar = "am", Nvar = "mpg"))
  expect_error(ExpCustomStat(mtcars))
  expect_error(ExpCustomStat(mtcars, Cvar = "am", Nvar = "mpg"))
  expect_error(ExpCustomStat(mtcars, Nvar = "mpg", gpby = FALSE))
})

test_that("test output object", {
  catstat <- ExpCustomStat(mtcars, Cvar = "am", Nvar = "mpg", stat = "mean")
  str(catstat)
  expect_output(str(catstat), "data.frame")
  expect_output(str(catstat), "2 obs")
  catstat1 <- ExpCustomStat(mtcars, Nvar = "mpg", stat = "mean")
  expect_equal(catstat1$mean, mean(mtcars$mpg))
})
