#' Customized summary statistics
#'
#' Table of descriptive statistics. Output returns matrix object containing descriptive information on all input variables for each level or combination of levels in categorical/group variable. Also while running the analysis user can filter out the data by individual variable level or across data level.
#'
#' @param data dataframe or Matrix
#' @param Cvar qualitative variables on which to stratify / subgroup or run categorical summaries
#' @param Nvar quantitative variables on which to run summary statistics for.
#' @param stat descriptive statistics. Sepecify which summary statistics required (Included all base stat functions like 'mean','medain','max','min','sum','IQR','sd','var',quantile like P0.1, P0.2 etc'). Also added two more stat here are 'PS' is percentage of shares and 'Prop' is column percentage
#' @param gpby default value is True. Group level summary will be created based on list of categorical variable. If summary required at each categorical variable level then keep this option as FALSE
#' @param filt filter out data while running the summary statistics. Filter can apply accross data or individual variable level using filt option. If there are multiple filters, seperate the conditons by using '^'. Ex: Nvar = c("X1","X2","X3","X4"), let say we need to exclude data X1>900 for X1 variable, X2==10 for X2 variable, Gender !='Male' for X3 variable and all data for X4 then filt should be, filt = c("X1>900"^"X2==10"^"Gender!='Male'"^all) or  c("X1>900"^"X2==10"^"Gender!='Male'"^ ^). in case if you want to keep all data for some of the variable listed in Nvar, then specify inside the filt like ^all^ or ^ ^(single space)
#' @param dcast fast dcast from data.table
#' @param value If dcast is TRUE, pass the variable name which needs to come on column
#' @return
#' summary statistics as dataframe. Usage of this function is detailed in user guide vignettes document.
#' @details
#' Filter unique value from all the numeric variables
#'
#' Case1: Excluding unique values or outliers values like '999' or '9999' or '888' etc from each selected variables.
#'
#' Eg:dat = data.frame(x = c(23,24,34,999,12,12,23,999,45),
#'                  y = c(1,3,4,999,0,999,0,8,999,0)
#'
#' Exclude 999:
#'
#' x = c(23,24,34,12,12,23,45)
#'
#' y = c(1,3,4,0,0,8,0)
#'
#' Case2: Summarise the data with selected descriptive statistics like 'mean' and 'median' or 'sum' and 'variance' etc..
#'
#' Case3: Aggregate the data with different statistics using group by statement
#'
#' Case4: Reshape the summary statistics.. etc
#'
#' The complete functionality of `ExpCustomStat` function is detailed in vignette help page with example code.
#' @examples
#' ## Selected summary statistics 'Count,sum, percentage of shares' for
#' ## disp and mpg variables by vs, am and gear
#' ExpCustomStat(mtcars, Cvar=c("vs","am","gear"), Nvar = c("disp","mpg"),
#'              stat = c("Count","sum","PS"), gpby = TRUE, filt = NULL)
#'
#' ExpCustomStat(mtcars, Cvar=c("gear"), Nvar = c("disp","mpg"),
#'              stat = c("Count","sum","var"), gpby = TRUE, filt = "am==1")
#'
#' ExpCustomStat(mtcars, Cvar = c("gear"), Nvar = c("disp","mpg"),
#'              stat = c("Count","sum","mean","median"), gpby = TRUE, filt = "am==1")
#'
#' ## Selected summary statistics 'Count and fivenum stat for disp and mpg
#' ## variables by gear
#' ExpCustomStat(mtcars, Cvar = c("gear"), Nvar = c("disp", "mpg"),
#'               stat = c("Count",'min','p0.25','median','p0.75','max'), gpby = TRUE)
#'
#' @export ExpCustomStat
#' @import data.table
#' @importFrom data.table dcast .N setDT
#' @importFrom stats formula

ExpCustomStat <- function(data, Cvar=NULL, Nvar=NULL, stat=NULL, gpby=TRUE,
                          filt=NULL, dcast=FALSE, value=NULL){
  Nvartp <- 0
  if (is.null(data) | !is.data.frame(data))
    stop("input data is missing | not a data frame")
  if (is.null(Cvar) & is.null(Nvar)) stop("Input variables are missing")
  if (!is.null(Cvar) & !is.null(Nvar) & is.null(stat))
    stop("'stat' value is NULL, Specify the required statistics")
  setDT(data)
  ## Group by
  if (is.null(Cvar)) {
    Cv <- "Overall"
    data[, Overall := "All"]
  } else {
    Cv <- Cvar
    }
  if (is.null(Nvar)) {
    Nvar <- "TP"
    data[, TP := 0]
    if (is.null(stat)) stat <- c("Count", "Prop")
    Nvartp <- 1
  } else {
    num_var <- Nvar[sapply(data[, Nvar, with = F], is.numeric)]
    not_num <- Nvar[Nvar %ni% num_var]
    if (length(not_num) >= 1)  cat(paste0(not_num, collapse = ", "),
                                   "variable/s not in numeric type", "\n",
                                   "Convert into numeric or remove from 'Nvar' list",
                                   "\n\n")
    if (length(not_num) == 1 & length(Nvar) == 1)
      stop(cat(not_num, "is not numeric \n"))
    Nvar <- Nvar
  }
  ## With roup by statement
  if (isTRUE(gpby)) {
    Final_data <- NULL
    fk <- 0L
    for (k in Nvar) {
      if (sapply(data[, k, with = F], is.numeric) == F) next
      fk <- fk + 1L
      if (is.null(filt)) {
        data1 <- data;
        filter <- "NA"
      } else {
        datfun <- ctab_filter(data, filt, fk, k)
        data1 <- datfun[["dataset"]]
        filter <- datfun[["filter"]]
      }
      nn <- 0L
      summatab <- ctab_stat(data1, stat, k, Cv, filter)

      Final_data <- rbind(Final_data, summatab)
      rm(summatab, nn)
    }
    if (is.null(filt)) {
      Final_data[, Filter := NULL]
      }
    if (Nvartp > 0) {
      Final_data[, Attribute := NULL]
      }
    if (is.null(Cvar)) {
      Final_data[, Overall := NULL]
      }

    nam1 <- stat
    nam2 <- names(Final_data)[names(Final_data) %ni% nam1]
    Final_data <- Final_data[, c(nam2, nam1), with = F]

  if (isTRUE(dcast)) {
      if (is.null(Cvar))
        stop("if dcast option is TRUE then 'Cvar' should not be empty")
      if (!is.null(value) & (is.null(Nvar) | length(Nvar) == 1)){

        colun_nam <- value
        Rownam <- Cvar[ !(Cvar %in% value)]
        cf <- formula(paste(paste0(colun_nam, collapse = "+"),
                            paste0(Rownam, collapse = "+"), sep = "~"))
        cp <- dcast(Final_data, cf, value.var = stat)
        cat(paste0("Row value   : ", paste0(colun_nam, collapse = " + "),
                   "\n", "Column value: ", paste0(Rownam, collapse = " + "),
                   "\n", "Statistics  : ", paste0(stat, collapse = " + "),
                   "\n", "\n"))
        return(cp)
      } else
        if (length(Nvar) >= 1) {
          colun_nam <- "Attribute"
          Rownam <- Cvar
          cf <- formula(paste(paste0(colun_nam, collapse = "+"),
                              paste0(Rownam, collapse = "+"), sep = "~"))
          cp <- dcast(Final_data, cf, value.var = stat)
          cat(paste0("Row value   : ", paste0(colun_nam, collapse = " + "),
                     "\n", "Column value: ", paste0(Rownam, collapse = " + "),
                     "\n", "Statistics  : ", paste0(stat, collapse = " + "),
                     "\n", "\n"))
          return(cp)
        } else
          if (!(is.null(Cvar)) & is.null(Nvar)) {
            print("Input error")
          } else {
            print("No reshape for this input, correct the input")
            }
    } else {
      return(Final_data)
      }
    } else
  if (gpby == FALSE) {
      if (is.null(Cvar)) stop("Group variable is missing")
    Final_data1 <- NULL
    for (g in Cv) {
      Final_data <- NULL
      fk <- 0L
      for (k in Nvar) {
        if (sapply(data[, k, with = F], is.numeric) == F) next
        fk <- fk + 1L
        if (is.null(filt)) {
          data1 <- data
          filter <- "NA"
        } else {
          datfun <- ctab_filter(data, filt, fk, k)
          data1 <- datfun[["dataset"]]
          filter <- datfun[["filter"]]
        }
        nn <- 0L
        summatab <- ctab_stat(data1, stat, k, g, filter)

        Final_data <- rbind(Final_data, summatab)
        rm(summatab, sum1, nn)
      }
      setnames(Final_data, g, "Level")
      Final_data[, Group_by := g]
      Final_data1 <- rbind(Final_data1, Final_data)
      rm(Final_data)
    }
    if (is.null(filt)) {
      Final_data1[, Filter := NULL]
      }
    if (Nvartp > 0) {
      Final_data1[, Attribute := NULL]
      }
    if (is.null(Cvar)) {
      Final_data1[, Group_by := NULL]
      Final_data[, Overall := NULL]
      }
    nam1 <- stat
    nam2 <- names(Final_data1)[names(Final_data1) %ni% nam1]
    Final_data1 <- Final_data1[, c(nam2, nam1), with = F]
    if (isTRUE(dcast)){
      if (is.null(Cvar)) stop("Group variable 'Cvar' is null")
      cf <- formula(paste("Attribute", paste0(c("Group_by", "Level"),
                                              collapse = "+"), sep = "~"))
      cp <- dcast(Final_data1, cf, value.var = stat)
      return(cp)
    } else {
      return(Final_data1)
      }
  }
}

## To avoid the cran error checks
globalVariables(c(":=", ".", "Overall", "TP", "Group_by",
                  "Attribute", "x", "sum1"))
