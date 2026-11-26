#' perform validateScenarios and create an .html report using .Rmd templates
#'
#' @param dataPath one or multiple path(s) to scenario data in .mif or .csv
#'        format
#' @param config name a config from inst/config ("validationConfig_<name>.csv")
#'        or give a full path to a separate configuration file
#' @param report name a .Rmd from inst/markdown ("validationReport_<name>.Rmd")
#'        to be rendered or give a full path to a separate .Rmd file
#' @param outputDir choose a directory to save validation reports to
#' @param extraColors if TRUE, use cyan and blue for violation of min thresholds
#'        instead of using the same colors as for max thresholds (yel and red)
#' @param giveSummary print a summary of input data via ``showInputSummary()``
#'        which allows spotting data inconsistencies
#'
#' @export

validationReport <- function(dataPath, config,
                             report = "default",
                             outputDir = "output",
                             extraColors = TRUE,
                             giveSummary = FALSE) {

  # rendering packages are only suggested, as they are not needed for
  # validateScenarios(); further packages loaded within the individual
  # templates are listed in Suggests as well
  missingPkgs <- Filter(function(p) !requireNamespace(p, quietly = TRUE),
                        c("rmarkdown", "knitr"))
  if (length(missingPkgs) > 0) {
    stop("validationReport() requires the following package(s): ",
         paste(missingPkgs, collapse = ", "), ". Please install them.")
  }

  # detect if dataPath is file path or R object
  if (is.character(dataPath)) {
    # convert relative to absolute paths
    dataPath <- normalizePath(dataPath)
  } else {
    # use object name if possible
    dataName <- deparse(substitute(dataPath))
    if (length(dataName) != 1 || dataName == "") dataName <- "R_object"
  }

  # user has the option to enter name of files that are shipped with package
  # or provide full paths to manually created files for config and report
  if (is.character(config) &&
      file.exists(normalizePath(config, mustWork = FALSE))) {
    # full path to config given
    config <- normalizePath(config)
    configName <- "Custom"
  } else if (is.character(config)) {
    # name of config file in inst/config given
    configName <- config
  } else {
    # config provided as R object (tibble or df)
    configName <- "Custom"
  }

  # report
  if (is.character(report) &&
      file.exists(normalizePath(report, mustWork = FALSE))) {
    # full path to report given
    reportPath <- normalizePath(report)
    reportName <- "Custom"
  } else {
    # name of report file in inst/markdown given
    reportPath <- system.file(
      paste0("markdown/validationReport_", report, ".Rmd"),
      package = "piamValidation")
    if (file.exists(reportPath)) {
      reportName <- report
    } else {
      stop(paste("Report .Rmd not found!\n",
                 "Requested:", report, "\n\n",
                 "Available reports:\n",
                 paste(listReports(), collapse = "\n")))
    }
  }

  # put rendered reports in output folder in working directory
  outputPath <- paste0(getwd(), "/", outputDir)
  if (!dir.exists(outputPath)) dir.create(outputPath)

  # include chosen config and report name in output file except if it is default
  infix <- ""
  if (configName != "default") infix <- paste0(infix, "_cfg-", configName)
  if (reportName != "default") infix <- paste0(infix, "_rep-", reportName)

  # render in a job-specific temporary directory: knitr writes intermediate
  # files next to the input file, but the package library may be read-only
  # and parallel renders of the same template would collide
  tmpDir <- file.path(tempdir(), format(Sys.time(), "render_%Y%m%d%H%M%S"))
  dir.create(tmpDir, recursive = TRUE, showWarnings = FALSE)
  tmpReport <- file.path(tmpDir, basename(reportPath))
  file.copy(reportPath, tmpReport, overwrite = TRUE)

  # create specified report for given data and config
  yamlParams <- list(mif = dataPath, cfg = config, extraColors = extraColors)
  rmarkdown::render(input = tmpReport,
                    params = yamlParams,
                    output_file = paste0(outputPath, "/validation", infix,
                                         format(Sys.time(), "_%Y%m%d-%H%M%S"),
                                         ".html"))
}

