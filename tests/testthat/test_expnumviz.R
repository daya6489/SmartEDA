context("Distributions of numerical variables")

test_that("test normal functions", {
  expect_error(ExpNumViz(data = iris, target = "Species", col = rainbow(2),
                         sample = 1, scatter = TRUE))
  expect_error(ExpNumViz(data = iris, target = "Petal.Length", scatter = TRUE))
  expect_message(ExpNumViz(data = iris, target = "Species", col = "yellow"))

})

test_that("test input object", {
  vector <- rnorm(10, mean = 0.5, sd = 0.003)
  expect_error(ExpNumViz(data = vector))
  df <- data.frame(Xvar = c(rep("A", 3), rep("B", 3), rep("C", 3)),
                  Yvar = c(rep("SD", 2), rep("PP", 2), rep("kk", 5)))
  expect_error(ExpNumViz(data = df))
  expect_error(ExpNumViz(data = iris, Page = 1))

})

test_that("test output object", {
  plotlst <- ExpNumViz(iris)
  expect_is(plotlst, "list")
  expect_null(names(plotlst))
  expect_true(is.ggplot(plotlst[[1]]))
})
