context("Distributions of categorical variables")

test_that("test target and color inputs", {
  expect_error(ExpCatViz(data = mtcars, target = "gear", col = rainbow(4)))
  expect_message(ExpCatViz(data = mtcars, target = "mpg"))
})


test_that("test output object", {
  plotlst <- ExpCatViz(mtcars)
  expect_is(plotlst, "list")
  expect_null(names(plotlst))
  expect_true(is.ggplot(plotlst[[1]]))
})
