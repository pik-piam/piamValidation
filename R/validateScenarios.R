#' performs the validation checks from a config on a scenario data set
#'
#' @param dataPath one or multiple path(s) to scenario data in .mif or .csv
#'        format, in case of historic comparison, also path to reference data
#' @param config select config from inst/config or give a full path to a config
#'        file on your computer
#' @param outputFile give name of output file in case results should be exported;
#'        include file extension
#' @param extraColors if TRUE, use cyan and blue for violation of min thresholds
#'        instead of using the same colors as for max thresholds (yel and red)
#' @param giveSummary print a summary of input data via ``showInputSummary()``
#'        which allows spotting data inconsistencies
#'
#' @importFrom dplyr filter select mutate group_by %>% bind_rows
#'
#' @export
validateScenarios <- function(dataPath, config,
                              outputFile = NULL,
                              extraColors = TRUE,
                              giveSummary = FALSE) {

  # get config first so that only the variables it needs are imported
  cfg <- getConfig(config)

  data <- importScenarioData(dataPath, variables = unique(cfg$variable))

  # historical/reference data has to have "historical" as scenario name
  hist <- filter(data, scenario == "historical")
  scen <- filter(data, scenario != "historical")
  rm(data)

  # convert config into a format that is easy to handle
  cfg <- cfg %>%
    fillInf() %>%
    expandPeriods(scen) %>%
    expandVariables(scen)
  cfg[cfg == "NA"] <- NA

  if (giveSummary) showInputSummary(scen, hist, cfg)

  # warn if reference data required by the config is missing from input data
  checkRefData(cfg, hist)

  # filter data for variables from config
  hist <- filter(hist, variable %in% unique(cfg$variable))
  scen <- filter(scen, variable %in% unique(cfg$variable))

  # combine scenario data (and reference data if needed) with the respective
  # thresholds for each row of the config and bind all into one data.frame
  valiData <- bind_rows(
    lapply(1:nrow(cfg), function(i) {
      combineData(
        scen[scen$variable %in% cfg[[i, "variable"]], ],
        cfg[i, ],
        histData = filter(hist, variable %in% cfg[[i, "variable"]])
        )
      }))

  # "lower" rows of config overwrite "higher" rows when describing the same data
  valiData <- resolveDuplicates(valiData)

  # perform actual checks and write results in new columns of data.frame
  valiData <- evaluateThresholds(valiData, extraColors = extraColors)

  if (nrow(valiData) == 0) {
    stop("Something went wrong, returned data.frame is empty.")
  }

  # export validated data to file in case outputFile is specified
  if (!is.null(outputFile)) {
    # in case full path is given
    if (file.exists(outputFile)) {
      write.csv(valiData, outputFile, row.names = FALSE, quote = FALSE)
    } else {
      output_path <- paste0(path.package("piamValidation"), "/output")
      if (!dir.exists(output_path)) dir.create(output_path)
      write.csv(valiData, paste0(output_path, "/", outputFile),
                row.names = FALSE, quote = FALSE)
    }
  }

  return(valiData)
}



