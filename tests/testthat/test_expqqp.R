context("Parallel Co-ordinate plots")

test_that("test input object", {
  df <- data.frame(Xvar = c(rep("A", 3), rep("B", 3), rep("C", 3)),
                   Yvar = c(rep("SD", 2), rep("PP", 2), rep("kk", 5)))
  expect_error(ExpOutQQ(df, nlim = 10, fname = NULL,
                        Page = c(2, 2), sample = 4))
  expect_error(ExpOutQQ(mtcars$carb, nlim = 10, fname = NULL,
                        Page = c(2, 2), sample = 4))
})

test_that("test output object", {
  plotlst <- ExpOutQQ(mtcars, nlim = 10, fname = NULL,
                      Page = c(2, 2), sample = 4)
  expect_output(str(plotlst), "List")
  expect_equal(class(plotlst), "list")
})
