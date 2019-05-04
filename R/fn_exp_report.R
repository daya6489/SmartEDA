#' Function to create HTML EDA report
#'
#' @description Create a exploratory data analysis report in HTML format
#' @usage ExpReport(data,Template=NULL,Target=NULL,label=NULL,op_file=NULL,
#' op_dir=getwd(),sc=NULL,sn=NULL,Rc=NULL)
##' @param data a data frame
##' @param Template R markdown template (.rmd file)
##' @param Target dependent variable. If there is no defined target variable then keep as it is NULL.
##' @param label target variable descriptions, not a mandatory field
##' @param op_file output file name (.html)
##' @param op_dir output path
##' @param sc sample number of plots for categorical variable. User can decide how many number of plots to depict in html report.
##' @param sn sample number of plots for numerical variable. User can decide how many number of plots to depict in html report.
##' @param Rc reference category of target variable. If Target is categorical then Pclass value is mandatory and which should not be NULL
##' @seealso \code{\link[DataExplorer:create_report]{create_report}}
##' @details
##' The "ExpReport" function will generate a HTML report for any R data frames.
##' If the markdown template is ready, we can use that template to generate the HTML report else
##' It will generate three different types of HTML report based on the Target field
##'
##' IF Target = NULL, means there is no defined dependent varaible then it will genreate general EDA report at overall level
##'
##' IF Target = continuous, then it will genreate EDA report including univariate and multivarite summary statistics with correlation.
##'
##' IF Target = categorical, then it will genreate EDA report including univariate and multivarite summary statistics with chi-square, Information values.
##' @importFrom rmarkdown render
##' @importFrom utils browseURL
##' @export ExpReport

ExpReport<-function (data,Template=NULL,Target=NULL,label=NULL,op_file=NULL,op_dir=getwd(),sc=NULL,sn=NULL,Rc=NULL)
{
  data=as.data.frame(data)
  if(!is.null(Template)) fil4 = paste0(Template)

  fil1 = paste0("rmd_template/report_tmp.Rmd")
  fil2 = paste0("rmd_template/report_tmp_1.Rmd")
  fil3 = paste0("rmd_template/report_tmp_2.Rmd")


  if (!is.null(Target) & is.null(Rc)) message("'Rc' is missing, if target variable is categorical then define the reference value in Pclass")

  if (!is.null(Template)) {pathname = fil4}
  else
  if (is.null(Target)){pathname = fil2}
  else
    {
      Yvar = data[,Target]
      if(is.numeric(Yvar) & length(unique(Yvar))>5) { pathname = fil1
      } else {pathname = fil3}
      }

  if(is.null(op_file)) stop("Output file name is missing")
  args <- as.list(match.call())

  if (!is.null(Template)) {report_dir=pathname} else
  {report_dir <- system.file(pathname, package = "SmartEDA")}
  suppressWarnings(render(input = report_dir, output_file = op_file,
                          output_dir = op_dir, intermediates_dir = op_dir,
                          params = list(data = data, Target = Target)))
  report_path <- file.path(op_dir, op_file)
  browseURL(report_path)
  if (ifelse(is.null(args[["quiet"]]), TRUE, !args[["quiet"]]))
    message(paste0("\n\nReport is generated at \"", report_path,"\"."))
}
