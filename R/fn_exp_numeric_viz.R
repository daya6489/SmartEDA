#' Distributions of numeric variables
#'
#' This function automatically scans through each variable and creates density plot, scatter plot and box plot for continuous variable.
#'
#' @param data dataframe or matrix
#' @param target target variable
#' @param type 1 (boxplot by category and overall), 2 (boxplot by category only), 3 (boxplot for overall)
#' @param nlim numeric variable unique limit. Default nlim is 3, graph will exclude the numeric variable which is having less than 'nlim' unique value
#' @param fname output file name
#' @param col define the fill color for box plot. Number of color should be equal to number of categories in target variable
#' @param Page output pattern. if Page=c(3,2), It will generate 6 plots with 3 rows and 2 columns
#' @param sample random selection of plots
#' @param scatter option to run scatter plot between all the numerical variables (default scatter=FALSE)
#' @param gtitle chart title
#' @param theme adding extra themes, geoms, and scales for 'ggplot2' (eg: themes options from ggthemes package)
#' @seealso
#' \code{\link[ggplot2:geom_boxplot]{geom_boxplot}}
#' \code{\link[ggthemes:ggthemes]{ggthemes}}
#' \code{\link[ggplot2:geom_density]{geom_density}}
#' \code{\link[ggplot2:geom_point]{geom_point}}
#'
#' @details
#' This function automatically scan each variables and generate a graph based on the user inputs. Graphical representation includes scatter plot, box plot and density plots.
#'
#' \itemize{
#'   \item \code{target} is continuous then output is scatter plots
#'   \item \code{target} is categorical then output is box plot
#'   \item \code{target} is NULL then density plot for all numeric features
#'   \item \code{scatter = TRUE} generate multiple scatter plot between all the independent contionuos variables with or without group argument
#'}
#'
#' @return returns collated graphs in PDF or JPEG format
#'
#' \itemize{
#'   \item \code{Univariate plot} density plot for all the numeric data with the value of shape of the distribution (Skewness & Kurtosis)
#'   \item \code{Bivariate plot} correlatin plot for all the numeric data
#'   \item \code{Bivariate plot} scatter plot between continuous dependent variable and Independent variables
#'   \item \code{Box plot} by overall sample
#'   \item \code{Box plot} by stratified sample
#'}
#' @importFrom grDevices colors
#' @importFrom gridExtra marrangeGrob
#' @importFrom sampling srswor
#' @importFrom utils combn
#' @examples
#' ## Generate Boxplot by category
#' ExpNumViz(iris,target = "Species", type = 2, nlim = 2,
#'            col = c("red", "green", "blue", "pink"), Page = NULL, sample = 2, scatter = FALSE,
#'            gtitle = "Box plot: ")
#' ## Generate Density plot
#' ExpNumViz(iris, nlim = 2,
#'            col = NULL,Page = NULL, sample = 2, scatter = FALSE,
#'            gtitle = "Density plot: ")
#' ## Generate Scatter plot by Dependent variable
#' ExpNumViz(iris,target = "Sepal.Length", type = 1, nlim = 2,
#'            col = "red", Page = NULL, sample = NULL, scatter = FALSE,
#'            gtitle = "Scatter plot: ", theme = "Default")
#' ## Generate Scatter plot for all the numerical variables
#' ExpNumViz(iris,target = "Species", type = 1, nlim = 2,
#'            col = c("red", "green", "blue"), Page = NULL, sample = NULL, scatter = TRUE,
#'            gtitle = "Scatter plot: ", theme = "Default")
#' @export ExpNumViz

ExpNumViz <- function (data, target = NULL, type = 1, nlim = 3, fname = NULL,
                       col = NULL, Page = NULL, sample = NULL, scatter = FALSE,
                       gtitle = NULL, theme = "Default"){
  if (!is.data.frame(data)) stop("data must be a numeric vector or data.frame")
  xx <- as.data.frame(data)
  num_var <- names(xx)[sapply(xx, is.numeric)]
  wrap_40 <- wrap_format(40)
  if (length(num_var) == 0) stop("there is no numeric variable in the data frame")

  if (length(num_var) > 0){
    if (length(num_var) == 1) {
      xx1 <- as.data.frame(xx[, num_var]); names(xx1) <- num_var
      } else {
      xx1 <- xx[, num_var]
      }
    num_var <- num_var[sapply(xx1, function(x) {
      length(unique(na.omit(x))) >= nlim
      })]
  }

  if (!is.null(sample)) {
    if (sample > length(num_var))
      stop("Sample number is greater than counts of variables")
    num_var <- num_var[srswor(sample, length(num_var)) == 1]
  }
  if (isTRUE(scatter)) {
    if (length(num_var) < 2) stop("Input data has less than 2 variables")
    plot_comb <- combn(num_var, 2)
  }
  # Target variable is not defined
  if (is.null(target)) {
    if (isTRUE(scatter)) {
      fill_1 <- scolorsel(col, nlevel = 1)
      if (length(fill_1) > 1) stop("length of colour input must be 1")
        plot_l <- lapply(c(1:ncol(plot_comb)), function(x) {
        xi <- plot_comb[, x][1]
        yi <- plot_comb[, x][2]
        xd <- na.omit(subset(xx, select = c(xi, yi)))
        names(xd) <- c("XX", "YY")
        p <- ggplot(xd, aes(x = XX, y = YY)) +
          geom_point(colour = fill_1, size = 2) + xlab(xi) + ylab(yi) +
          ggtitle(wrap_40(paste(gtitle, " ", xi, " vs ", yi))) + smtheme(theme)
        return(p)
      })
    }
    else {
      plot_l <- lapply(num_var, function(j) {
        x <- na.omit(subset(xx, select = j))
        y <- xx[, j]
        p <- ggplot(data = x, aes_string(x = names(x))) +
          geom_line (stat = "density", size = 1, alpha = 1) +
          xlab (paste0( (colnames(x)), "\n", "Skewness: ",
                      round(ExpSkew(y, type = "moment"), 2), " Kurtosis: ",
                      round(ExpKurtosis(y, type = "excess"), 2))) +
          smtheme(theme)
        return(p)
      })
    }
  }
  # Target variable is defined
  else {
    if (isTRUE(scatter)) {
      target1 <- xx[, target]
      nlevel <- length(unique(target1))
      fill_1 <- scolorsel(col, nlevel)
      if (length(unique(na.omit(target1))) <= 20) {
        plot_l <- lapply(c(1:ncol(plot_comb)), function(x){
          xi <- plot_comb[, x][1]
          yi <- plot_comb[, x][2]
          xd <- na.omit(subset(xx, select = c(xi, yi, target)))
          names(xd) <- c("XX", "YY", "ZZ")
          xd$ZZ <- as.factor(paste0(xd$ZZ))
          p <- ggplot(xd, aes(x = XX, y = YY)) +
            geom_point(size = 2, aes(color = ZZ)) +
            scale_color_manual(name = target, values = fill_1) +
            xlab(xi) + ylab(yi) +
            ggtitle(wrap_40(paste(gtitle, " ", xi, " vs ", yi))) +
            smtheme(theme)
          return(p)
        }
        )
      } else stop("If scatter option is TRUE then 'target should be categorical' else 'change scatter = FALSE' ")
    }
    else {
      target1 <- xx[, target]
      if (is.numeric(target1) & length(unique(na.omit(target1))) >= 6) {
        if (is.null(col)) col <- "#5F9EA0"
        if (length(col) > 1) stop("defined more than one colour")
        num_var1 <- num_var[!num_var %in% target]
        comma <- NULL
        plot_l <- lapply(num_var1, function(j) {
          x <- na.omit(subset(xx, select = c(j, target)))
          ggplot(x, aes_string(x = names(x)[2], y = names(x)[1])) +
            geom_point(colour = col, size = 2) + scale_x_continuous(labels = comma) +
            scale_y_continuous(labels = comma) + smtheme(theme)
          })
      }
      else {
        target1 <- as.factor(as.character(paste0(target1)))
        plot_l <- lapply(num_var, function(j) {
          mdat <- subset(xx, select = c(target, j))
          names(mdat) <- c("GP", "NV")
          if (type == 1) {
            mdat$GP <- as.character(paste0(mdat$GP))
            if (anyNA(mdat$GP)) {
              mdat$GP <- addNA(mdat$GP)
            }
            mdat1 <- mdat
            mdat1$GP <- "All"
            gdata <- rbind(mdat, mdat1)
          }
          if (type == 2) {
            gdata <- mdat
            gdata$GP <- as.character(paste0(gdata$GP))
            if (anyNA(gdata$GP)) {
              gdata$GP <- addNA(mdat$GP)
            }
          }
          if (type == 3) {
            gdata <- mdat
            gdata$GP <- "ALL"
          }

          nlevel <- length(unique(gdata$GP))
          fill_1 <- scolorsel(col, nlevel)
          if ( !is.null(col)) {
            if (length(col) != nlevel) message("Insufficient values in colour, number of colours should be equal to number of categories")
            }
          gg1 <- ggplot(gdata, aes(y = NV, x = GP)) +
            geom_boxplot(fill = fill_1) + xlab(target) +
            ylab(j) + ggtitle(wrap_40(paste(gtitle, " ", j, " vs ", target))) +
            scale_x_discrete(labels = wrap_format(8)) +
            theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size = 8, colour = "grey20"),
                  plot.title = element_text(hjust = 0.5, face = "bold", colour = "#5F9EA0", size = 12)) + smtheme(theme)
          return(gg1)
        })
      }
    }
  }
  if (!is.null(fname)) {
    swritepdf(fname, plot_l, Page)
  }  else {
    if (!is.null(Page)) {
      pn <- length(plot_l)
      nc <- Page[2]
      nr <- Page[1]
      if ( (nc * nr) > pn + 3)
        stop("reduce the matrix dimension from Page(r,c)")
      gspl <- split(plot_l, (seq_along(plot_l) - 1) %/% pn)
      gplt <- lapply(gspl, function(g) marrangeGrob(grobs = g,
                                                    layout_matrix = matrix(data = seq(1, pn),
                                                                           nrow = nr, ncol = nc)))
      return(gplt)
      } else {
        return(plot_l)
      }
  }
}

  globalVariables(c("comma", "XX", "YY", "ZZ", "GP", "NV"))
