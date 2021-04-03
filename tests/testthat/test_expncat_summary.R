context("Frequency and custom tables")

test_that("test normal function", {
  expect_error(ExpCTable(mtcars, Target = "mpg", margin = 1, clim = 10,
                         nlim = 3, bin = 100, per = FALSE))
})

test_that("test input object", {
  expect_error(ExpNumViz(data = vector))
  df <- data.frame(Xvar = c(rep("A", 2), rep("B", 2), rep("C", 2)),
                   Yvar = c(rep("SD", 2), rep("PP", 4)))
  ft <- ExpCTable(df)
  tot_freq <-  ft$Frequency[4]
  expect_equal(tot_freq, 6)
  expect_length(ft$Frequency, 7)

})

test_that("test output object", {
  plotlst <- ExpCTable(mtcars)
  str(plotlst)
  expect_output(str(plotlst), "21 obs")
  expect_output(str(plotlst), "5 variables")
  expect_true(is.data.frame(plotlst))
  expect_true(is.data.frame(plotlst))
  expect_s3_class(plotlst, "SmartEDA")
})
