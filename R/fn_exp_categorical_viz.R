#' Distributions of categorical variables
#'
#'This function automatically scans through each variable and creates bar plot for categorical variable.
#'
#' @param data dataframe or matrix
#' @param target target variable. This is not a mandatory field
#' @param fname output file name. Output will be generated in PDF format
#' @param clim maximum categories to be considered to include in bar graphs
#' @param col define the colors to fill the bars, default it will take sample colours
#' @param margin index, 1 for row based proportions and 2 for column based proportions
#' @param Page output pattern. if Page=c(3,2), It will generate 6 plots with 3 rows and 2 columns
#' @param Flip default vertical bars. It will be used to flip the axis vertical to horizontal
#' @param sample random selection of categorical variable
#' @param rdata to plot bar graph for frequency/aggregated table
#' @param value value coloumn name. This is mandatory if 'rdata' is TRUE
#' @param gtitle graph title
#' @param theme adding extra themes, geoms, and scales for 'ggplot2' (eg: themes options from ggthemes package)
#' @seealso
#' \code{\link[ggplot2:geom_bar]{geom_bar}}
#'
#' @return This function returns collated graphs in grid format in PDF or JPEG format. All the files will be stored in the working directory
#'
#' \itemize{
#'   \item \code{Bar graph} for raw data(this function will dynamically pick all the categorical variable and plot the bar chart)
#'   \item \code{Bar graph} for aggregated data
#'   \item \code{Bar graph} is a Stacked Bar graph by target variable
#'}
#'
#' @examples
#' ## Bar graph for specified variable
#' mtdata = mtcars
#' mtdata$carname = rownames(mtcars)
#' ExpCatViz(data=mtdata,target="carname",col="blue",rdata=TRUE,value="mpg")
#' n=nrow(mtdata)
#' ExpCatViz(data=mtdata,target="carname",col=rainbow(n),rdata=TRUE,value="mpg") ## Ranibow colour
#' # Stacked bar chart
#' ExpCatViz(data=mtdata,target = "gear",col=hcl.colors(3, "Set 2"))
#' ExpCatViz(data=mtdata,target = "gear",col=c("red", "green", "blue"))
#' # Bar chart
#' ExpCatViz(data=mtdata)
#' ExpCatViz(data=mtdata,col="blue",gtitle = "Barplot")
#' @importFrom grDevices colors pdf dev.off
#' @importFrom gridExtra marrangeGrob
#' @importFrom sampling srswor
#' @export ExpCatViz

ExpCatViz <- function (data, target = NULL, fname = NULL, clim = 10, col = NULL, margin = 1,
                       Page = NULL, Flip = F, sample = NULL, rdata = FALSE, value = NULL,
                       gtitle = NULL, theme = "Default") {
  if (!is.data.frame(data)) stop("'data must be a data.frame'")
  if (is.null(data))stop("There is no input data frame")
  r <- 0
  xx <- as.data.frame(data)
  if (rdata == FALSE) {
    num_var <- names(xx)[sapply(xx, is.numeric)]
    Cat_var <- c(names(xx)[sapply(xx, is.character)], names(xx)[sapply(xx, is.factor)])
    if (length(num_var) > 0) {
      if (length(num_var) == 1) {
      xx1 <- as.data.frame(xx[, num_var])
      names(xx1) <- num_var
      } else {
        xx1 <- xx[, num_var]
        }
      num_var <- num_var[sapply(xx1, function(x){
        length(unique(na.omit(x))) > 1 & length(unique(na.omit(x))) <= clim
        })]
    }
    if ( (length(num_var) + length(Cat_var)) == 0) stop("there is no categorical variable in the data")
    wrap_40 <- wrap_format(40)
    if (!is.null(target)) {
      if (!target %in% names(xx)) stop("undefined columns selected for target")
      Yvar <- as.character(paste0(xx[, target]))
      nlevel <- length(unique(Yvar))
      if (!is.null(col) & length(col) != nlevel) stop("Insufficient values in colour, number of colours should be equal to number of categories")
      if (nlevel > 15) message("target variable has more than 15 categories")
      fill_1 <- scolorsel(col, nlevel)
      if (length(Cat_var) > 0){
        if (length(Cat_var) == 1) {
          xx1 <- as.data.frame(xx[, Cat_var])
          names(xx1) <- Cat_var
        } else {
          xx1 <- xx[, Cat_var]
          }
        Cat_varlst <- Cat_var[sapply(xx1, function(x){
          length(unique(x)) <= clim & length(unique(x)) >= 2
          })]
        Cat_varlst <- c(Cat_varlst, num_var)
        Cat_varlst <- Cat_varlst[!(Cat_varlst %in% target)]
        } else {
          Cat_varlst <- num_var[!(num_var %in% target)]
          }
    }
    else {
      if (length(Cat_var) > 0){
        if (length(Cat_var) == 1) {
          xx1 <- as.data.frame(xx[, Cat_var])
          names(xx1) <- Cat_var
        } else {
          xx1 <- xx[, Cat_var]
          }
        Cat_varlst <- Cat_var[sapply(xx1, function(x){
          length(unique(x)) <= clim & length(unique(x)) >= 2
          })]
        Cat_varlst <- c(Cat_varlst, num_var)
      } else Cat_varlst <- num_var
    }
    if (!is.null(sample)) {
      if (sample > length(Cat_varlst)) stop("Sample number is greater than counts of variables")
      Cat_varlst <- Cat_varlst[srswor(sample, length(Cat_varlst)) == 1]
    }
  } else {
    if (is.null(value)) stop("value column is missing")
    Cat_varlst <- value
  }
  ### GGPLOT graph start from here
  plot_l <- lapply(Cat_varlst, function(j){
    Xvar <- as.character(paste0(xx[, j]))
    if (rdata == TRUE){
      data <- as.data.frame(data)
      tbl <- data[, c(target, value)]
      names(tbl) <- c("Xvar", "Freq")
      tbl$Freq <- round(tbl$Freq, 1)
      nlevel <- length(unique(tbl$Xvar))
      fill_1 <- scolorsel(col, nlevel)
      pp <- ggplot(tbl, aes(y = Freq, x = reorder(Xvar, Freq), label = paste0(Freq))) +
        geom_bar(stat = "identity", position = "dodge", fill = fill_1) + xlab("") + ggtitle(gtitle) +
        ylab(value) + geom_text(size = 3, position = position_dodge(width = 0.9), vjust = 0) +
        scale_x_discrete(labels = wrap_format(8)) +
        scale_y_continuous(labels = dollar_format(suffix = "", prefix = "")) + smtheme(theme)
      if (Flip == TRUE) {
        return(pp + coord_flip())
        } else {
          return(pp)
        }
    }
    if (!is.null(target)) {
        Tar_var <- xx[, target]
        if (is.numeric(Tar_var) & length(unique(Tar_var)) > 5) {
          message("Target variable is ignored and ploting the univarite bar plot")
          nlevel <- length(unique(Xvar))
          fill_1 <- scolorsel(col, nlevel)
          tb <- table(Xvar)
          tbl <- data.frame(round( tb / sum(tb) * 100, r))
           pp <- ggplot(tbl, aes(y = Freq, x = Xvar, label = paste0(Freq, "%"))) +
            geom_bar( stat = "identity", position = "dodge", fill = fill_1) + xlab("") +
            ylab("Percentage (%)") + ggtitle(wrap_40(paste(gtitle, " ", j))) +
            geom_text(size = 4, position = position_dodge(width = 0.9), vjust = 0) +
            scale_x_discrete(labels = wrap_format(8)) +
            scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) + smtheme(theme)
          if (Flip == TRUE) {
            return(pp + coord_flip())
            } else return(pp)
        } else {
          tb <- table(Xvar, Yvar)
          Freq <- NULL
          switch (margin,
                  tbl <- data.frame(round(prop.table(tb, 2) * 100, r)),
                  tbl <- data.frame(round(prop.table(tb, 1) * 100, r)))
          pp <- ggplot(tbl, aes(fill = Yvar, y = Freq, x = Xvar, label = paste0(Freq, "%"))) +
            geom_bar(stat = "identity", position = "dodge") + xlab("") +
            ylab("Percentage (%)") + ggtitle(wrap_40(paste(gtitle, " ", j, " vs ", target, "[Target]"))) +
            geom_text(size = 4, position = position_dodge(width = 0.9), vjust = 0) +
            scale_x_discrete(labels = wrap_format(8)) +
            scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
            scale_fill_manual("Target", values = fill_1) + smtheme(theme)
          if (Flip == TRUE) {
            return(pp + coord_flip())
          } else {
              return(pp)
            }
        }
      } else {
      nlevel <- length(unique(Xvar))
      fill_1 <- scolorsel(col, nlevel)
      tb <- table(Xvar)
      tbl <- data.frame(round(tb / sum(tb) * 100, r))
      pp <- ggplot(tbl, aes(y = Freq, x = Xvar, label = paste0(Freq, "%"))) +
        geom_bar( stat = "identity", position = "dodge", fill = fill_1) + xlab("") +
        ylab("Percentage (%)") + ggtitle(wrap_40(paste(gtitle, " ", j))) +
        geom_text(size = 4, position = position_dodge(width = 0.9), vjust = 0) +
        scale_x_discrete(labels = wrap_format(8)) +
        scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) + smtheme(theme)
      if (Flip == TRUE) {
        return(pp + coord_flip())
      } else {
          return(pp)
        }
    }
  })
  if (!is.null(fname)) {
    swritepdf(fname, plot_l, Page)
  }  else {
    if (!is.null(Page)) {
    pn <- length(plot_l)
    nc <- Page[2]
    nr <- Page[1]
    if ( (nc * nr) > pn + 3) stop("reduce the matrix dimension from Page(r,c)")
    gspl <- split(plot_l, (seq_along(plot_l) - 1) %/% pn)
    gplt <- lapply(gspl, function(g) marrangeGrob(grobs = g, nrow = nr, ncol = nc))
    return(gplt)
  } else {
      return(plot_l)
    }
  }
}
# Glabal variables
globalVariables(c("Freq", "Xvar", "Yvar"))
