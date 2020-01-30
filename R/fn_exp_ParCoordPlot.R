#' Parallel Co ordinate plots
#'
#' This function creates parallel Co ordinate plots
#'
#' @param data Input dataframe or data.table
#' @param Group stratification variables
#' @param Stsize vector of startum sample sizes
#' @param Nvar vector of numerice variables, default it will consider all the numeric variable from data
#' @param Cvar vector of categorical variables, default it will consider all the categorical variable
#' @param scale scale the variables in the parallel coordinate plot (Default normailized with minimum of the variable is zero and maximum of the variable is one) (see ggparcoord details for more scale options)
#' @seealso \code{\link[GGally:ggparcoord]{ggparcoord}}
#' @return  Parallel Co ordinate plots
#'
#' @details
#' The Parallel Co ordinate plots having the functionalities of visulization for sample rows if data size large. Also data can be stratified basis of Target or group variables. It will normalize all numeric variables between 0 and 1 also having other standardization options. It will automatically make dummy (1,0) variables for categorical variables
#'
#' @examples
#' CData = ISLR::Carseats
#' # Defualt ExpParcoord funciton
#' ExpParcoord(CData,Group=NULL,Stsize=NULL,
#' 			   Nvar=c("Price","Income","Advertising","Population","Age","Education"))
#' # With Stratified rows and selected columns only
#' ExpParcoord(CData,Group="ShelveLoc",Stsize=c(10,15,20),
#' 			   Nvar=c("Price","Income"),Cvar=c("Urban","US"))
#' # Without stratification
#' ExpParcoord(CData,Group="ShelveLoc",Nvar=c("Price","Income"),
#' 			   Cvar=c("Urban","US"),scale=NULL)
#' # Scale changed std: univariately, subtract mean and divide by standard deviation
#' ExpParcoord(CData,Group="US",Nvar=c("Price","Income"),
#' 			   Cvar=c("ShelveLoc"),scale="std")
#' # Selected numeric variables
#' ExpParcoord(CData,Group="ShelveLoc",Stsize=c(10,15,20),
#' 			   Nvar=c("Price","Income","Advertising","Population","Age","Education"))
#'
#' @importFrom GGally ggparcoord
#' @importFrom sampling strata
#' @export ExpParcoord

ExpParcoord <- function(data, Group = NULL, Stsize = NULL,
                        Nvar = NULL, Cvar = NULL, scale = NULL){
  if ( !is.data.frame(data)) stop("data must be a numeric vector or data.frame")
  xx <- as.data.frame(data)
  if (!is.null(Stsize) & is.null(Group)) stop("Group variable is missing for stratified sampling")
  if (!is.null(Group) & !is.null(Stsize)) Stratified <- TRUE else Stratified <- FALSE

  # Strata : Sampling
  if (Stratified == TRUE) {
    s <- strata(xx, Group, Stsize, "srswor")
    rowref <- s$ID_unit
    xx <- xx[rowref, ]
  }

  # Variable selection
  num_var <- names(xx)[sapply(xx, is.numeric)]
  Cat_var <- c(names(xx)[sapply(xx, is.character)], names(xx)[sapply(xx, is.factor)])
  if (is.null(Nvar) & is.null(Cvar)){
    Numvar <- num_var
    Catvar <- Cat_var
    if (!is.null(Group)){
      df <- cbind(xx[, Numvar], make_dummies(xx[, Catvar]), Group = as.factor(xx[, Group]))
    } else {
      df <- cbind(xx[, Numvar], make_dummies(xx[, Catvar]))
    }
  } else
    if (!is.null(Nvar) & !is.null(Cvar)) {
      if (!is.null(Group)) {
        df <- cbind(xx[, Nvar], make_dummies(xx[, Cvar]), Group = as.factor(xx[, Group]))
      } else {
        df <- cbind(xx[, Nvar], make_dummies(xx[, Cvar]))
        }
    } else
    if (!is.null(Nvar) & is.null(Cvar)){
      Numvar <- Nvar
      if (!is.null(Group)){
        df <- cbind(xx[, Numvar], Group = as.factor(xx[, Group]))
      } else {
        df <- cbind(xx[, Numvar])
      }
    } else
    if (is.null(Nvar) & !is.null(Cvar)) {
      Catvar <- Cvar
      if (!is.null(Group)) {
        df <- cbind(xx[, Catvar], Group = as.factor(xx[, Group]))
      } else {
        df <- cbind(make_dummies(xx[, Catvar]))
      }
    } else stop("No variables")
  # Group variable index
  if (!is.null(Group)){
    gcol <- ncol(df)
    np <- gcol - 1
    cnum <- 1 : np
  } else {
    cnum <- 1 : ncol(df)
    gcol <- NULL
  }
  # Scale variable
  if (is.null(scale)){
    scalep <- "uniminmax"
  } else {
    scalep <- scale
  }
  # Coordinate plot
  p <- ggparcoord(data = df, columns = cnum, groupColumn = gcol,
                 scale = scalep, showPoints = TRUE, title = "Parallel Coordinate Plot",
                 alphaLines = 0.3) + theme_bw() +
    scale_x_discrete(labels = wrap_format(6))
  return(p)
}
