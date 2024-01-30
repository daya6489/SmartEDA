#' Univariate Outlier Analysis
#'
#' this function will run univariate outlier analysis based on boxplot or SD method. The function returns the summary of oultlier for selected numeric features and adding new features if there is any outliers
#'
#' @param data dataframe or matrix
#' @param varlist list of numeric variable to perform the univariate outlier analysis
#' @param method detect outlier method boxplot or NxStDev (where N is 1 or 2 or 3 std deviations, like 1xStDev or 2xStDev or 3xStDev)
#' @param treatment treating outlier value by mean or median. default NULL
#' @param capping default LL = 0.05 & UL = 0.95cap the outlier value by replacing those observations outside the lower limit with the value of 5th percentile and above the upper limit, with the value of 95th percentile value
#' @param outflag add extreme value flag variable into output data
#' @details this function provides both summary of the outlier variable and data
#'
#' Univariate outlier analysis method
#' \itemize{
#'   \item \code{boxplot} is If a data value are below (Q1 minus 1.5x IQR) or boxplot lower whisker or above (Q3 plus 1.5x IQR) or boxplot upper whisker then those points are flaged as outlier value
#'   \item \code{Standard Deviation} is If a data distribution is approximately normal then about 68 percent of the data values lie within one standard deviation of the mean and about 95 percent are within two standard deviations, and about 99.7 percent lie within three standard deviations.  If any data point that is more than 3 times the standard deviation, then those points are flaged as outlier value
#' }
#' @return Outlier summary includes
#'
#' \itemize{
#'   \item \code{Num of outliers} is Number of outlier in each variable
#'   \item \code{Lower bound} is Q1 minus 1.5x IQR for boxplot; Mean minus 3x StdDev for Standard Deviation method
#'   \item \code{Upper bound} is Q3 plus 1.5x IQR for boxplot; Mean plus 3x StdDev for Standard Deviation method
#'   \item \code{Lower cap} is Lower percentile capping value
#'   \item \code{Upper cap} is Upper percentile capping value
#' }
#'
#' @examples
#' ExpOutliers(mtcars, varlist = c("mpg","disp","wt", "qsec"), method = 'BoxPlot',
#' capping = c(0.1, 0.9), outflag = TRUE)
#'
#' ExpOutliers(mtcars, varlist = c("mpg","disp","wt", "qsec"), method = '2xStDev',
#' capping = c(0.1, 0.9), outflag = TRUE)
#'
#' # Mean imputation or 5th percentile or 95th percentile value capping
#' ExpOutliers(mtcars, varlist = c("mpg","disp","wt", "qsec"), method = 'BoxPlot',
#' treatment = "mean", capping = c(0.05, 0.95), outflag = TRUE)
#'
#' @import data.table
#' @importFrom data.table setDT setDF
#' @import ISLR
#' @importFrom stats quantile IQR median sd
#' @export ExpOutliers

ExpOutliers <- function(data, varlist = NULL, method = "boxplot", treatment = NULL, capping = c(0.05, 0.95), outflag = FALSE){

  if(is.null(data))stop("There is no input data frame")
  if(!is.data.frame(data)) stop("'data must be a data.frame'"); setDT(data)
  if(is.null(varlist)) stop("'varlist' is mandatory, please enter the list of variables for outlier testing")

  ## final output data frame variable list
  final_varlst <- names(data)
  outlier_variable <- intersect(final_varlst, varlist)
  varnotava <- setdiff(varlist, final_varlst)
  if(length(outlier_variable) <= 0) stop("Check the input variable list or Input has no numeric variables")
  if(length(varnotava)>0) stop(paste0("Variable ",paste0(varnotava,", ")," not avaialble in the input dataframe"))
  method <- tolower(method)
    flag_function <- function(x, y = NULL) {
      qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
      capval <- quantile(x, probs=capping, na.rm = T)
      Lcap <- capval[[1]]
      Ucap <- capval[[2]]

      if(method == "boxplot"){
        iqr_v <- 1.5 * IQR(x, na.rm = TRUE)
        Lower_bound <- round(qnt[[1]] - 1.5 * IQR(x, na.rm = TRUE), 2)
        Upper_bound <- round(qnt[[2]] + 1.5 * IQR(x, na.rm = TRUE), 2)
      } else
        {
          sdv <- as.numeric(substr(method,1,1))
          if(!is.numeric(sdv)) stop("selected outlier method is wrong")
          std_dev <- sd(x, na.rm = TRUE)
          mean_value <- mean(x, na.rm = TRUE)
          anomaly_cut <- std_dev * sdv
          Lower_bound <- round(mean_value - anomaly_cut, 2)
          Upper_bound <- round(mean_value + anomaly_cut, 2)
        }
      outrows_lower <-paste(which(x < Lower_bound), collapse = ",")
      outrows_upper <-paste(which(x > Upper_bound), collapse = ",")
      num_outlier <- length(x[x<Lower_bound | x>Upper_bound])
      mean_b <- round(mean(x, na.rm = T), 2)
      mean_a <- round(mean(x[x >= Lower_bound & x <= Upper_bound], na.rm = T), 2)
      median_b <- median(x, na.rm = T)
      median_a <- median(x[x >= Lower_bound & x <= Upper_bound], na.rm = T)

      if(y =='summary'){
        op <- c(Lcap, Ucap, Lower_bound, Upper_bound, num_outlier, outrows_lower, outrows_upper, mean_b, mean_a, median_b,median_a)
        return(op)
      } else
        if (num_outlier>0){
        if(y =='flag'){
          op <- ifelse(x < Lower_bound | x > Upper_bound, 1, 0)
        } else
          if(y =='impute'){
            value <- mean_a
            if(treatment == "median") value <- median_a
            op <- ifelse(x < Lower_bound | x > Upper_bound, value, x)
          } else
            if(y =='capping'){
              op <- ifelse(x < Lower_bound, Lcap, ifelse(x > Upper_bound, Ucap, x))
            }
          return(op)
          }
    }

## Outlier analysis summary
      cpval <- data[, lapply(.SD, function(x) {
        flag_function(x, y = "summary")
      }),  .SDcols = outlier_variable]
      cpval[, Category:= c(paste0("Lower cap : ", capping[1]), paste0("Upper cap : ", capping[2]), "Lower bound", "Upper bound", "Num of outliers", "Lower outlier case", "Upper outlier case", "Mean before", "Mean after","Median before", "Median after")]
      cpval <- cpval[,c("Category", varlist), with = F]

      setDF(cpval)
      varlist_new <- varlist[sapply(cpval[cpval$Category == 'Num of outliers', varlist], function(x) x>0)]
      #varlist_new <- outlier_variable
      if(length(varlist_new) > 0){
        ## Outlier treatment - capping
        data[, paste0("out_cap_", varlist_new):= lapply(.SD, function(x) {
          flag_function(x, y = "capping")
        }),  .SDcols = varlist_new]

        ## Outlier treatment - flag
        if(outflag== TRUE){
          data[, paste0("out_flag_", varlist_new):= lapply(.SD, function(x) {
            flag_function(x, y = "flag")
          }),  .SDcols = varlist_new]
        }

        ## Outlier treatment - impute
        if(!is.null(treatment)) {
          if(! treatment %in% c("mean", "median")) stop("Outlier treatment method either mean or median")
          data[, paste0("out_imp_", varlist_new):= lapply(.SD, function(x) {
            flag_function(x, y = "impute")
          }),  .SDcols = varlist_new]
        }
        uoi <- lapply(cpval[7, -1], function(x) {as.numeric(strsplit(x, ",", fixed = TRUE)[[1]])})
        loi <- lapply(cpval[6, -1], function(x) {as.numeric(strsplit(x, ",", fixed = TRUE)[[1]])})
        return(list("outlier_summary" = cpval, "outlier_data" = data, "outlier_index" = list("upper_out_index" = uoi, "lower_out_index" = loi)))
      } else {
        return(cpval)
      }
}

## To avoid the cran error checks
globalVariables(c("Category"))
