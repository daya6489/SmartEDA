context("Export html report")

path_name <- getwd()
file_name <- "testthat_expreport.html"
file_dir <- file.path(path_name, file_name)

test_that("test overall exp report function", {
  skip_on_cran()
  expect_error(ExpReport(mtcars$mpg, Template = NULL, Target = "gear",
                         label = NULL, theme = "Default", op_file = file_name,
                         op_dir = path_name, sc = 2, sn = 2, Rc = 4))
  expect_error(ExpReport(mtcars$mpg, Template = NULL, Target = "gear",
                         label = NULL, op_dir = path_name,
                         sc = 2, sn = 2, Rc = 4))
  if (file.exists(file_dir)) file.remove(file_dir)
})

# test_that("test if output file is generated", {
#   skip_on_cran()
#   ExpReport(mtcars, Template = NULL, Target = "gear", label = NULL,
#             theme = "Default", op_file = file_name, op_dir = path_name,
#             sc = 2, sn = 2, Rc = 4)
#   expect_true(file.exists(file_dir))
#   dp <- dir(path = path_name, pattern = ".html")
#   expect_gte(length(dp), 1)
#   if (file.exists(file_dir)) file.remove(file_dir)
#
# })
#
# test_that("test if report is generated", {
#   skip_on_cran()
#   ExpReport(iris, Template = NULL, label = NULL, theme = "Default",
#             op_file = file_name, op_dir = path_name)
#   expect_true(file.exists(file_dir))
#   expect_gte(file.info(file_dir)$size, 10000)
#   if (file.exists(file_dir)) file.remove(file_dir)
# })
