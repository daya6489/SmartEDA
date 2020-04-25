context("Generate two independent plots")

test_that("test overall exp report function", {
  skip_on_cran()
  expect_error(ExpTwoPlots(data,plot_type = "categorical",iv_variables = NULL,target = NULL,
                lp_geom_type = 'boxplot',lp_arg_list = list(),rp_geom_type = 'boxplot',
                rp_arg_list = list(), theme = "Default"))
  })
