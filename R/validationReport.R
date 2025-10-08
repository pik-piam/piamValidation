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
#'
#' @importFrom piamutils getSystemFile
#'
#' @export

validationReport <- function(dataPath, config, report = "default",
                             outputDir = "output", extraColors = TRUE) {

  # convert relative to absolute paths
  dataPath <- normalizePath(dataPath)

  # user has the option to enter name of files that are shipped with package
  # or provide full paths to manually created files for config and report

  if (file.exists(normalizePath(config, mustWork = F))) {
    # full path to config given
    config <- normalizePath(config)
    configName <- "Custom"
  } else {
    # name of config file in inst/config given
    configName <- config
  }

  if (file.exists(normalizePath(report, mustWork = F))) {
    # full path to report given
    reportPath <- normalizePath(report)
    reportName <- "Custom"
  } else {
    # name of report file in inst/markdown given
    reportPath <- piamutils::getSystemFile(
      paste0("markdown/validationReport_", report, ".Rmd"),
      package = "piamValidation")
    if (file.exists(reportPath)) {
      reportName <- report
    } else {
      available <- list.files(system.file("markdown", package = "piamValidation"))
      stop(paste("Report .Rmd not found! \n
                 Available markdown files:\n",
                 available))
    }

  }

  # put rendered reports in output folder in working directory
  outputPath <- paste0(getwd(), "/", outputDir)
  if (!dir.exists(outputPath)) dir.create(outputPath)

  # include chosen config and report name in output file except if it is default
  infix <- ""
  if (configName != "default") infix <- paste0(infix, "_cfg-", configName)
  if (reportName != "default") infix <- paste0(infix, "_rep-", reportName)

  # create specified report for given data and config
  yamlParams <- list(mif = dataPath, cfg = config, extraColors = extraColors)
  rmarkdown::render(input = reportPath,
                    params = yamlParams,
                    output_file = paste0(outputPath, "/validation", infix,
                                         format(Sys.time(), "_%Y%m%d-%H%M%S"),
                                         ".html"))
}

