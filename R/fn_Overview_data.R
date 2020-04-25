#' Function to generate the overview of a data frame
#'
#' This function used to produce summaries of data structure and overview of the data frame.
#'
#' @param data a data frame
#' @param type Type 1 is overview of the data; Type 2 is structure of the data
#' @details
#' This function provides overview and structure of the data frames.
#' \itemize{
#' \item Type = 1, overeview of the data (column names are "Descriptions", "Obs")
#' \item Type = 2, structure of the data (column names are "S.no", "Variable Name", "Variable Type", "% of Missing", "No. of Unique values")
#'}
#' @examples
#' # Overview of the data
#' ExpData(data=mtcars,type=1)
#' # Structure of the data
#' ExpData(data=mtcars,type=2)
#' @export ExpData

ExpData <- function(data, type = 1){
  if (!is.data.frame(data)) stop("data must be a numeric vector or data.frame")
  xx <- as.data.frame(data)
  dd <- sapply( sapply(xx, function(x){
    round((length(x[is.na(x)]) + length(x[x=='']))/ length(x), 5)
    }),
    function(y) {
      if (y == 0.0) xx <- 1
      else if (y > 0.9) xx <- 2
      else if (y >= 0.5) xx <- 3
      else if (y < 0.5) xx <- 4
      }
    )

  p1 <- paste0(round(length(dd[dd == 4]) / length(dd) * 100, 2), "%", " (", length(dd[dd == 4]), ")")
  p2 <- paste0(round(length(dd[dd == 3]) / length(dd) * 100, 2), "%", " (", length(dd[dd == 3]), ")")
  p3 <- paste0(round(length(dd[dd == 2]) / length(dd) * 100, 2), "%", " (", length(dd[dd == 2]), ")")
  p4 <- paste0(round(length(dd[dd == 1]) / length(dd) * 100, 2), "%", " (", length(dd[dd == 1]), ")")

  Date_cnt <- length(names(xx)[unlist(sapply(xx, function(x) class(x) %in% c("Date", "POSIXct", "POSIXt")))])
  Unif_cnt <- length(names(xx)[unlist(sapply(xx, function(x) length(unique(x[!is.na(x)])) == 1))])
  Unvar_cnt <- length(names(xx)[unlist(sapply(xx, function(x) length(unique(x)) == length(x)))])

     if (type == 1){
       Out_put <- data.frame (rbind(
            c("Sample size (Nrow)", nrow(xx)),
            c("No. of Variables (Ncol)", ncol(xx)),
            c("No. of Numeric Variables", length(names(xx)[sapply(xx, is.numeric)])),
            c("No. of Factor Variables", length(names(xx)[sapply(xx, is.factor)])),
            c("No. of Text Variables", length(names(xx)[sapply(xx, is.character)])),
            c("No. of Logical Variables", length(names(xx)[sapply(xx, is.logical)])),
            c("No. of Unique Variables", Unvar_cnt),
            c("No. of Date Variables", Date_cnt),
            c("No. of Zero variance Variables (Uniform)", Unif_cnt),
            c("%. of Variables having complete cases", p4),
            c("%. of Variables having <50% missing cases", p1),
            c("%. of Variables having >50% missing cases", p2),
            c("%. of Variables having >90% missing cases", p3)
            )
            )
          names(Out_put) <- c("Descriptions", "Obs")
          return(Out_put)
          }
     if (type == 2){
            ## Data Structure
            name_var <- names(xx)
            tt <- sapply(name_var, function(x){
              Xvar <- xx[, x]
              cla_var <- as.character(paste0(class(xx[, x]), collapse = ":"))
              Per_missing <- round((length(Xvar[is.na(Xvar)]) + length(Xvar[Xvar=='']))/ length(Xvar), 5)
              Per_Unique <- length(unique(Xvar))
              mydata <- c(S.no = 1, VarName = x, VarClass = cla_var, Per_mis = Per_missing,
                          Unique = Per_Unique, VarDescriptions = NULL)
              return(mydata)
            }
            )

            op <- data.frame(t(tt))
            op$S.no <- seq(1, length(name_var), 1)
            rownames(op) <- NULL
            names(op) <- c("S.no", "Variable Name", "Variable Type", "% of Missing", "No. of Unique values")
            op[, 2] <- as.character(paste0(op[, 2]))
            op[, 3] <- as.character(paste0(op[, 3]))
            op[, 4] <- as.numeric(paste0(op[, 4]))
            op[, 5] <- as.integer(paste0(op[, 5]))
            return(op)
        }
}
