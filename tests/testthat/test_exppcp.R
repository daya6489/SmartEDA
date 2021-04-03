context("Parallel Co-ordinate plots")

test_that("test input object", {
  expect_error(ExpParcoord(mtcars, Stsize = 20,
                           Nvar = c("mpg", "disp", "wt", "gear")))
})

test_that("test output object", {
  plotlst <- ExpParcoord(mtcars, Nvar = c("mpg", "disp", "wt", "gear"))
  expect_is(plotlst, "ggplot")
})
