#' Summary statistics for numerical variables
#'
#' Function provides summary statistics for all numerical variable. This function automatically scans through each variable and select only numeric/integer variables. Also if we know the target variable, function will generate relationship between target variable and each independent variable.
#'
#' @param data dataframe or matrix
#' @param by group by A (summary statistics by All), G (summary statistics by group), GA (summary statistics by group and Overall)
#' @param gp target variable if any, default NULL
#' @param Qnt default NULL. Specified quantile is c(.25,0.75) will find 25th and 75th percentiles
#' @param Nlim numeric variable limit (default value is 3 which means it will only consider those variable having more than 3 unique values and variable type is numeric/integer)
#' @param MesofShape Measures of shapes (Skewness and kurtosis).
#' @param Outlier Calculate the lower hinge, upper hinge and number of outlier
#' @param round round off
#' @param weight a vector of weights, it must be equal to the length of data
#' @param dcast fast dcast from data.table
#' @param val Name of the column whose values will be filled to cast (see Details sections for list of column names)
#' @return summary statistics for numeric independent variables
#'
#' Summary by:
#'
#' \itemize{
#'   \item \code{Only overall level}
#'   \item \code{Only group level}
#'   \item \code{Both overall and group level}
#' }
#'
#'@details
#'
#' column descriptions
#' \itemize{
#'   \item \code{Vname} is Variable name
#'   \item \code{Group} is Target variable
#'   \item \code{TN} is Total sample (included NA observations)
#'   \item \code{nNeg} is Total negative observations
#'   \item \code{nPos} is Total positive observations
#'   \item \code{nZero} is Total zero observations
#'   \item \code{NegInf} is Negative infinite count
#'   \item \code{PosInf} is Positive infinite count
#'   \item \code{NA_value} is Not Applicable count
#'   \item \code{Per_of_Missing} is Percentage of missing
#'   \item \code{Min} is minimum value
#'   \item \code{Max} is maximum value
#'   \item \code{Mean} is average value
#'   \item \code{Median} is median value
#'   \item \code{SD} is Standard deviation
#'   \item \code{CV} is coefficient of variations (SD/mean)*100
#'   \item \code{IQR} is Inter quartile range
#'   \item \code{Qnt} is quantile values
#'   \item \code{MesofShape} is Skewness and Kurtosis
#'   \item \code{Outlier} is Number of outlier
#'   \item \code{Cor} is Correlation b/w target and independent variables
#' }
#'
#' @seealso \code{\link[psych:describe.by]{describe.by}}
#' @examples
#' # Descriptive summary of numeric variables is Summary by Target variables
#' ExpNumStat(mtcars,by="G",gp="gear",Qnt=c(0.1,0.2),MesofShape=2,
#'            Outlier=TRUE,round=3)
#' # Descriptive summary of numeric variables is Summary by Overall
#' ExpNumStat(mtcars,by="A",gp="gear",Qnt=c(0.1,0.2),MesofShape=2,
#'            Outlier=TRUE,round=3)
#' # Descriptive summary of numeric variables is Summary by Overall and Group
#' ExpNumStat(mtcars,by="GA",gp="gear",Qnt=seq(0,1,.1),MesofShape=1,
#'            Outlier=TRUE,round=2)
#' # Summary by specific statistics for all numeric variables
#' ExpNumStat(mtcars,by="GA",gp="gear",Qnt=c(0.1,0.2),MesofShape=2,
#'            Outlier=FALSE,round=2,dcast = TRUE,val = "IQR")
#' # Weighted summary statistics
#' ExpNumStat(mtcars,by="GA",gp="gear",Qnt=c(0.1,0.2),MesofShape=2,
#'            Outlier=FALSE,round=2,dcast = TRUE,val = "IQR", weight = "wt")
#'
#' @importFrom stats quantile median IQR var reorder sd cor
#' @importFrom data.table dcast.data.table setDT
#' @export ExpNumStat

ExpNumStat <- function(data, by = "A", gp = NULL, Qnt = NULL, Nlim = 10, MesofShape = 2,
                      Outlier = FALSE, round = 3, weight = NULL, dcast = FALSE, val = NULL) {
  if (is.data.frame(data) == is.numeric(data)) stop("data must be a numeric vector or data.frame")
  if (!MesofShape %in% c(1, 2)) stop("value of MesofShape should be either 1 or 2")

  if (!is.null(weight)) {
    if(length(intersect(colnames(data), weight))>0) {
      if(!is.numeric(data[[weight]])) stop("Weight column is not numeric")
      data$sm_9xy_1wt8 = data[[weight]]
    } else
      if((length(weight) == nrow(data))) {
        data$sm_9xy_1wt8 = weight
      } else
        stop("length of weight vector should be equal to sample size 'n' OR specified weight column is not there in dataframe")

  }

  wt <- NULL
  r <- round
  if (is.numeric(data)) {
    desc_sum <- ds_fun(x, r = r, MesofShape, Qnt, Outlier, weight)
    return(desc_sum)
    } else {
    xx <- as.data.frame(data)
    if (is.null(by)) by <- "A"
    if (!by %in% c("A", "G", "GA")) stop("by label should be like A, G or GA")
    if (!is.null(gp)) {
      grp <- xx[, gp]
      if (is.numeric(grp) & length(unique(grp)) > 5) {
        by <- "corr"
      } else {
        grp <- unique(as.character(paste0(xx[, gp])))
      }
    }

    if (is.null(gp) & by %in% c("G", "GA")) stop("gp variable is missing for group level summary")

    num_var <- names(xx)[sapply(xx, is.numeric)]

    if (length(num_var) == 0) stop("there is no numeric variable in the data frame")

    if (length(num_var) > 0) {
      if (length(num_var) == 1) {
        xx1 <- as.data.frame(xx[, num_var])
        names(xx1) <- num_var
        } else {
        xx1 <- xx[, num_var]
        }
      num_var <- num_var[sapply(xx1, function(x) {
        length(unique(na.omit(x))) >= Nlim
        })]
    }
    num_var = setdiff(num_var, c("sm_9xy_1wt8", "wt"))

    if (by == "A"){
      if (!is.null(weight)) wt = xx$sm_9xy_1wt8
      ccc <- sapply(xx[, num_var], function(x) ds_fun(x, r = r, MesofShape, Qnt, Outlier, weight = wt), USE.NAMES = TRUE)
      cname <- rownames(ccc)
      tb_op <- data.frame(t(ccc))
      names(tb_op) <- cname
      varn_gp <- cbind(Vname = rownames(tb_op), Group = "All")
      rownames(tb_op) <- NULL
      tb_op <- cbind(varn_gp, tb_op)
      tb_op <- tb_op[order(tb_op$Vname), ]
      class(tb_op) <- c("SmartEDA", "ExpNumStat", "data.frame")
    }
    if (by == "corr"){
      if (!is.null(weight)) wt = xx$sm_9xy_1wt8
        message(paste0("Note: Target variable is continuous", "\n", "Summary statistics excluded group by statement", "\n", "Results generated with correlation value against target variable"))
        ccc <- sapply(xx[, num_var], function(x) ds_fun(x, r = r, MesofShape, Qnt, Outlier, weight = wt), USE.NAMES = TRUE)
        cor <- round(cor(xx[, num_var])[, gp], r)
        ccc <- rbind(ccc, cor)
        cname <- rownames(ccc)
        tb_op <- data.frame(t(ccc))
        names(tb_op) <- cname
        varn_gp <- cbind(Vname = rownames(tb_op), Group = gp, Note = paste0("Cor b/w ", gp) )
        rownames(tb_op) <- NULL
        tb_op <- cbind(varn_gp, tb_op)
        tb_op <- tb_op[order(tb_op$Vname), ]
        class(tb_op) <- c("SmartEDA", "ExpNumStat", "data.frame")
      }
     if (by == "G") {
        tb_op <- data.frame()
        for (j in grp){
          xx_1 <- subset(xx, xx[, gp] == j)
          if (!is.null(weight)) wt = xx_1$sm_9xy_1wt8
          ccc <- sapply(xx_1[, num_var], function(x) ds_fun(x, r = r, MesofShape, Qnt, Outlier, weight = wt), USE.NAMES = TRUE)
          tcc <- data.frame(t(ccc))
          varn_gp <- cbind(Vname = rownames(tcc), Group = paste0(gp, ":", j))
          rownames(tcc) <- NULL
          tb_op1 <- cbind(varn_gp, tcc)
          cname <- c("Vname", "Group", rownames(ccc))
          names(tb_op1) <- cname
          tb_op <- rbind(tb_op, tb_op1)
        }
        tb_op <- tb_op[order(tb_op$Vname), ]
        class(tb_op) <- c("SmartEDA", "ExpNumStat", "data.frame")
      }
      if (by == "GA") {
          tb_op <- data.frame()
          grp <- c("All", grp)
          for (j in grp) {
            if (j == "All") {
              if (!is.null(weight)) wt = xx$sm_9xy_1wt8
              xx_1 <- subset(xx, select = num_var)
            } else {
              xx_1 <- subset(xx, xx[, gp] == j)
              if (!is.null(weight)) wt = xx_1$sm_9xy_1wt8
              }
            ccc <- sapply(xx_1[, num_var], function(x) ds_fun(x, r = r, MesofShape, Qnt, Outlier, weight = wt), USE.NAMES = TRUE)
            tcc <- data.frame(t(ccc))
            varn_gp <- cbind(Vname = rownames(tcc), Group = paste0(gp, ":", j))
            rownames(tcc) <- NULL
            tb_op1 <- cbind(varn_gp, tcc)
            cname <- c("Vname", "Group", rownames(ccc))
            names(tb_op1) <- cname
            tb_op <- rbind(tb_op, tb_op1)
          }
          tb_op <- tb_op[order(tb_op$Vname), ]
          names(tb_op) <- cname
          class(tb_op) <- c("SmartEDA", "ExpNumStat", "data.frame")
      }
     if (isTRUE(dcast)){
      cf <- formula(paste("Vname", "Group", sep = "~"))
      setDT(tb_op)
      cp <- data.frame(Stat = val, dcast.data.table(tb_op, cf, value.var = val))
      return(cp)
    } else return(tb_op)
  }
}

#' Measures of Shape - Skewness
#'
#'Measures of shape to give a detailed evaluation of data. Explains the amount and direction of skew. Kurotsis explains how tall and sharp the central peak is. Skewness has no units: but a number, like a z score
#'
#' @param x A numeric object or data.frame
#' @param type a character which specifies the method of computation. Options are "moment" or "sample"
#' @return ExpSkew returns Skewness values
#' @examples
#' ExpSkew(mtcars,type="moment")
#' ExpSkew(mtcars,type="sample")
#' @author dubrangala
#' @export ExpSkew

ExpSkew <- function(x, type){
  if (!is.data.frame(x)) {
    if (!is.numeric(x)) stop("The selected variable is not numeric")
    skw_op <- skew_fun(x)
    switch(type,
           moment = as.numeric(skw_op[1]),
           sample = as.numeric(skw_op[2]))
  } else {
    xx <- as.data.frame(x)
    num_var <- names(xx)[sapply(xx, is.numeric)]
    if (is.null(num_var)) stop("There is no numeric object found in data frame")
    num_var <- num_var[sapply(xx[, num_var], function(x){
      length(unique(na.omit(x))) > 3
      })]
    op_ut <- data.frame(sapply(xx[, num_var], function(x) skew_fun(x)))
    switch (type,
            moment = op_ut[1, ],
            sample = op_ut[2, ])
  }
}


#' Measures of Shape - Kurtosis
#'
#' Measures of shape to give a detailed evaluation of data. Explains the amount and direction of skew. Kurotsis explains how tall and sharp the central peak is. Skewness has no units: but a number, like a z score
#'
#' @param x A numeric object or data.frame
#' @param type a character which specifies the method of computation. Options are "moment" or "excess"
#' @return ExpKurtosis returns Kurtosis values
#' @examples
#' ExpKurtosis(mtcars$hp,type="excess")
#' ExpKurtosis(mtcars$carb,type="moment")
#' ExpKurtosis(mtcars,type="excess")
#' @author dubrangala
#' @export ExpKurtosis

ExpKurtosis <- function(x, type) {
  if (!is.data.frame(x)) {
    if (!is.numeric(x)) stop("The selected variable is not numeric")
    skw_op <-  Kurt_fun(x)
    switch(type,
           excess = as.numeric(skw_op[2]),
           moment = as.numeric(skw_op[1]))
  } else {
    xx <- as.data.frame(x)
    num_var <- names(xx)[sapply(xx, is.numeric)]
    if (length(num_var) < 1) stop("There is no numeric object found in data frame")
    num_var <- num_var[sapply(xx[, num_var], function(x){
      length(unique(na.omit(x))) > 3
      })]
    op_ut <- data.frame(sapply(xx[, num_var], function(x) Kurt_fun(x)))
    switch (type,
            excess = op_ut[2, ],
            moment = op_ut[1, ])
  }
}
