#' Quantile-Quantile Plots
#'
#' This function automatically scans through each variable and creates normal QQ plot also adds a line to a normal quantile-quantile plot.
#'
#' @param data Input dataframe or data.table
#' @param nlim numeric variable limit
#' @param fname output file name. Output will be generated in PDF format
#' @param Page output pattern. if Page=c(3,2), It will generate 6 plots with 3 rows and 2 columns
#' @param sample random number of plots
#' @seealso \code{\link[ggplot2:geom_qq]{geom_qq}}
#' @return  Normal quantile-quantile plot
#'
#' @examples
#' CData = ISLR::Carseats
#' ExpOutQQ(CData,nlim=10,fname=NULL,Page=c(2,2),sample=4)
#' @import ggplot2
#' @importFrom stats qnorm
#' @importFrom sampling srswor
#' @export ExpOutQQ

ExpOutQQ <- function(data, nlim = 3, fname = NULL, Page = NULL, sample = NULL){
  varb <- NULL
  if (!is.data.frame(data)) stop("data must be a numeric vector or data.frame")
  xx <- as.data.frame(data)
  num_var <- names(xx)[sapply(xx, is.numeric)]
  if (length(num_var) == 0) stop("there is no numeric variable in the data frame")
  num_var <- num_var[sapply(xx[, num_var], function(x){
    length(unique(na.omit(x))) >= nlim
    })]
  if (!is.null(sample)){
    if (sample > length(num_var)){
      num_var <- num_var
    } else {
      num_var <- num_var[srswor(sample, length(num_var)) == 1]
    }
  }

  ## QQ plot
  plot_l <- lapply(num_var, function(j){
    x <- na.omit(subset(xx, select = j))
    names(x) <- "varb"
    y <- na.omit(xx[, j])
    q25 <- quantile(y, 0.25, type = 5)
    q75 <- quantile(y, 0.75, type = 5)
    norm25 <- qnorm(0.25)
    norm75 <- qnorm(0.75)
    slope <- (q25 - q75) / (norm25 - norm75)
    int <- q25 - slope * norm25

    p <- ggplot(data = x, aes(sample = varb)) +
      stat_qq(distribution = qnorm, size = 2, alpha = 0.3, color = "blue") +
      geom_abline(intercept = int, slope = slope, col = "red", size = 0.5) +
      theme_bw() +
      xlab("Normal Quantiles") +
      ylab(j)
    return(p)
  }
  )
  if (!is.null(fname)) {
    swritepdf(fname, plot_l, Page)
  } else {
    if (!is.null(Page)){
      pn <- length(plot_l)
      nc <- Page[2]
      nr <- Page[1]
      if ( (nc * nr) > pn + 3) stop("reduce the matrix dimension from Page(r,c)")
      gspl <- split(plot_l, ( seq_along(plot_l) - 1) %/% pn)
      gplt <- lapply(gspl, function(g) marrangeGrob(grobs = g, layout_matrix = matrix(data = seq(1, pn), nrow = nr, ncol = nc)))
      return(gplt)
    } else {
      return(plot_l)
    }
  }
}
