#' Function to create HTML EDA report
#'
#' Create a exploratory data analysis report in HTML format
#'
#' @param data a data frame
#' @param Template R markdown template (.rmd file)
#' @param Target dependent variable. If there is no defined target variable then keep as it is NULL.
#' @param label target variable descriptions, not a mandatory field
#' @param theme customized ggplot theme (default SmartEDA theme) (for Some extra themes use Package: ggthemes)
#' @param op_file output file name (.html)
#' @param op_dir output path
#' @param sc sample number of plots for categorical variable. User can decide how many number of plots to depict in html report.
#' @param sn sample number of plots for numerical variable. User can decide how many number of plots to depict in html report.
#' @param Rc reference category of target variable. If Target is categorical then Pclass value is mandatory and which should not be NULL
#' @seealso \code{\link[DataExplorer:create_report]{create_report}}
#' @details
#' The "ExpReport" function will generate a HTML report for any R data frames.
#'
#' @note If the markdown template is ready, you can use that template to generate the HTML report
#'
#' @note ExpReport will generate three different types of HTML report based on the Target field
#' \itemize{
#' \item IF Target = NULL, means there is no defined dependent varaible then it will genreate general EDA report at overall level
#' \item IF Target = continuous, then it will genreate EDA report including univariate and multivarite summary statistics with correlation.
#' \item IF Target = categorical, then it will genreate EDA report including univariate and multivarite summary statistics with chi square, Information values.
#' }
#' @importFrom rmarkdown render
#' @importFrom utils browseURL
#' @examples
#' ## Creating HTML report
#' \dontrun{
#'  library (ggthemes)
#'  # Create report where target variable is categorical
#'  ExpReport(mtcars,Target="gear",label="car",theme=theme_economist(),op_file="Samp1.html",Rc=3)
#'  # Create report where target variable is continuous
#'  ExpReport(mtcars,Target="wt",label="car",theme="Default",op_file="Samp2.html")
#'  # Create report where no target variable defined
#'  ExpReport(mtcars,Target=NULL,label="car",theme=theme_foundation(),op_file="Samp3.html")
#' }
#' @export ExpReport

ExpReport <- function (data, Template = NULL, Target = NULL, label = NULL, theme = "Default",
                       op_file = NULL, op_dir = getwd(), sc = NULL, sn = NULL, Rc = NULL){
  if (!is.data.frame(data)) stop("Input data is not a dataframe")
  if (is.null(data)) stop("Input data is missing")
  data <- as.data.frame(data)

  if (is.null(op_file)) stop("Output file name is missing")
  if (!is.null(Template)) fil4 <- paste0(Template)

  fil1 <- paste0("rmd_template/report_tmp.Rmd")
  fil2 <- paste0("rmd_template/report_tmp_1.Rmd")
  fil3 <- paste0("rmd_template/report_tmp_2.Rmd")

  if (!is.null(Target) & is.null(Rc)) message("'Rc' is missing, if target variable is categorical then define the reference value in Pclass")

  if (!is.null(Template)) {
    pathname <- fil4
  } else
  if (is.null(Target)){
    pathname <- fil2
  } else {
      Yvar <- data[, Target]
      if (is.numeric(Yvar) & length(unique(Yvar)) > 5){
        pathname <- fil1
      } else {
        pathname <- fil3
        }
      }

  args <- as.list(match.call())

  if (!is.null(Template)) {
    report_dir <- pathname
  } else {
      report_dir <- system.file(pathname, package = "SmartEDA")
      }
  suppressWarnings(render(input = report_dir, output_file = op_file,
                          output_dir = op_dir, intermediates_dir = op_dir,
                          params = list(data = data, Target = Target)))
  report_path <- file.path(op_dir, op_file)
  browseURL(report_path)
  if (ifelse(is.null(args[["quiet"]]), TRUE, !args[["quiet"]]))
    message(paste0("\n\nReport is generated at \"", report_path, "\"."))
}
