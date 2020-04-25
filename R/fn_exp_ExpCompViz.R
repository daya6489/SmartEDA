#' Function to create two independent plots side by side for the same variable
#'
#' To plot graph from same variable when Target=NULL vs. when Target = categorical
#' variable (binary or multi-class variable)
#'
#' @param data dataframe
#' @param plot_type the plot type ("numeric", "categorical").
#' @param iv_variables list of independent variables. this input will be based off plot_type. List of numeric variables / List of categorical variables
#' @param target binary or multi-class dependent variable
#' @param fname output file name. Output will be generated in PDF format
#' @param lp_geom_type left side geom plot. this option is for univariate data. Options for numeric are "boxplot", "histogram", "density", "violin", "qqplot" and for categorical "bar", "pie", "donut"
#' @param lp_arg_list arguments to be passed to lp_geom_type. Default is list()
#' @param rp_geom_type right side geom plot. Options for numeric are "boxplot", "histogram", "density", "violin" "qqplot"  and for categorical "bar", "pie", "donut"
#' @param rp_arg_list arguments to be passed to rp_geom_type. Default is list()
#' @param page output pattern. if Page=c(3,2), It will generate 6 plots with 3 rows and 2 columns
#' @param theme adding extra themes, geoms, and scales for 'ggplot2' (eg: themes options from ggthemes package)
#'
#' @return This function returns same variable in two different views of ggplot in one graph. And there is a option to save the graph into PDF or JPEG format.
#'
#' @examples
#' ## Bar graph for specified variable
#' # Let's consider mtcars data set, it has several numerical and binary columns
#' target = "gear"
#' categorical_features <- c("vs", "am", "carb")
#' numeircal_features <- c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec")
#'
#' # plot numerical data two independent plots:
#' # Left side histogram chart wihtout target and Right side boxplot chart with target
#' num_1 <- ExpTwoPlots(mtcars, plot_type = "numeric",
#' iv_variables = numeircal_features, target = "gear",
#' lp_arg_list = list(alpha=0.5, color = "red", fill= "white",
#' binwidth=1),lp_geom_type = 'histogram',
#' rp_arg_list = list(fill = c("red", "green", "blue")),
#' rp_geom_type = 'boxplot', page = c(2,1),theme = "Default")
#'
#' # plot categorical data with two independent plots:
#' # Left side Donut chart wihtout target and Right side Stacked bar chart with target
#' cat_1 <- ExpTwoPlots(mtcars,plot_type = "categorical",
#' iv_variables = categorical_features,
#' target = "gear",lp_arg_list = list(),lp_geom_type = 'donut',
#' rp_arg_list = list(stat = 'identity', ),
#' rp_geom_type = 'bar',page = c(2,1),theme = "Default")
#'
#' @importFrom grDevices colors pdf dev.off
#' @importFrom gridExtra arrangeGrob
#' @importFrom utils head
#' @export ExpTwoPlots

ExpTwoPlots <- function(data, plot_type = "numeric", iv_variables = NULL,
                         target = NULL, lp_geom_type = "boxplot", lp_arg_list = list(),
                         rp_geom_type = "boxplot", rp_arg_list = list(), fname = NULL,
                         page = NULL, theme = "Default") {

  if (!is.data.frame(data)) stop("'data must be a data.frame'")
  if (is.null(data))stop("There is no input data frame")
  if (is.null(iv_variables)) stop("Independent variables (numeric / categorical) list is empty")
  if (plot_type == "numeric" & (lp_geom_type %in% c("bar", "barh", "pie", "donut")
                               | rp_geom_type %in% c("bar", "barh", "pie", "donut"))) stop("given geom type is not suitable for numerical IV variables, it will be boxplot, histogram etc..")
  if (plot_type == "categorical" & (!lp_geom_type %in% c("bar", "barh", "pie", "donut")
                               | !rp_geom_type %in% c("bar", "barh", "pie", "donut"))) stop("given geom type is not suitable for categorical IV variables, select either bar or pie chart")

  xx <- as.data.frame(data)
  xx <- subset(xx, select = c(iv_variables, target))
  if (!is.null(target)) xx[target] <- as.factor(xx[[target]])

### numeric independent variable plot
  if (plot_type == "numeric") {
    iv_variables <- names(xx)[sapply(xx, is.numeric)]
    if (length(iv_variables) == 0) stop("select one or more numeric independent variables")
    plot_l <- lapply(iv_variables, function(k) {

      lp <- univariate_num_plot(xx, var = k, arg_list = lp_arg_list, geom_type = lp_geom_type) +
        smtheme(theme)

      if (is.null(target)) {
        rp <- univariate_num_plot(xx, var = k, arg_list = rp_arg_list, geom_type = rp_geom_type) +
          smtheme(theme)

      } else {
        rp <- bivariate_num_plot(xx, target = target, var = k, arg_list = rp_arg_list, geom_type = rp_geom_type) +
          smtheme(theme)
      }

      plot <- arrangeGrob(lp, rp, nrow = 1, ncol = 2)
      return(plot)
      })
  } else

### Categorical independent variable plot
  if (plot_type == "categorical") {
    if (length(iv_variables) == 0) stop("select one or more categorical independent variables")
    xx[names(xx)] <- sapply(xx, as.factor)
    plot_l <- lapply(iv_variables, function(j) {
      lp <- univariate_cat_plot(xx, var = j, arg_list = lp_arg_list, geom_type = lp_geom_type) +
        smtheme(theme)
      if (is.null(target)) {
        rp <- univariate_cat_plot(xx, var = j, arg_list = rp_arg_list, geom_type = rp_geom_type) +
          smtheme(theme)

      } else {
        rp <- bivariate_cat_plot(xx, target = target, var = j, arg_list = rp_arg_list, geom_type = rp_geom_type) +
          smtheme(theme)
      }

      plot <- arrangeGrob(lp, rp, nrow = 1, ncol = 2)
      return(plot)

      })
  } else stop("Only types of plot types accepted numeric / categorical. Please correct the plot_type input")


  if (!is.null(fname)) {
    swritepdf(fname, plot_l, page)
  }  else {
    if (!is.null(page)) {
      pn <- length(plot_l)
      nc <- page[2]
      nr <- page[1]
      if ((nc * nr) > pn + 3) stop("reduce the matrix dimension from page(r,c)")
      gspl <- split(plot_l, (seq_along(plot_l) - 1) %/% pn)
      gplt <- lapply(gspl, function(g) marrangeGrob(grobs = g, nrow = nr, ncol = nc))
      return(gplt)
    } else {
      return(plot_l)
    }
  }

}
