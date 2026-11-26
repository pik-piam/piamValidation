#' List available configs
#'
#' List all validation configuration files that are delivered with the package
#' and can be directly imported with ``getConfig()`` or used in
#' ``validateScenarios()`` and ``validationReport()``.
#'
#' @returns character vector of config names
#'
#' @export
listConfigs <- function() {
  configs <- list.files(
    system.file("config", package = "piamValidation"),
    pattern = "^validationConfig_.*\\.csv$")
  sort(gsub("^validationConfig_|\\.csv$", "", configs))
}

#' List available reports
#'
#' List all validation report templates (.Rmd) that are delivered with the
#' package and can be used as ``report`` in ``validationReport()``.
#'
#' @returns character vector of report names
#'
#' @export
listReports <- function() {
  reports <- list.files(
    system.file("markdown", package = "piamValidation"),
    pattern = "^validationReport_.*\\.Rmd$")
  sort(gsub("^validationReport_|\\.Rmd$", "", reports))
}


#' Print a summary of the input data
#'
#' Print multiple metrics of all data objects given to ``validateScenarios()``
#' which might be helpful to spot and fix data inconsistencies.
#'
#' @param scen scenario data as used in ``validateScenarios()``
#' @param hist historical/reference data as used in ``validateScenarios()``
#' @param config config as used in ``validateScenarios()``
#'
#' @export
showInputSummary <- function(scen, hist, config) {
  nRows <- nrow(scen)
  nHist <- nrow(hist)
  nModels <- length(unique(scen$model))
  nScenarios <- length(unique(scen$scenario))
  nRegions <- length(unique(scen$region))
  nVariables <- length(unique(scen$variable))

  cfgVars <- unique(config$variable)
  scenVars <- unique(scen$variable)
  histVars <- unique(hist$variable)

  nCfgVars <- length(cfgVars)
  nMatchedScen <- sum(cfgVars %in% scenVars)
  nMatchedHist <- sum(cfgVars %in% histVars)

  # print summary
  message(paste(
    "\nValidation input summary:\n",
    "- Rows Scenario Data: ", nRows, "\n",
    "- Rows Historical Data: ", nRows, "\n",
    "- Models: ", nModels, "\n",
    "- Scenarios: ", nScenarios, "\n",
    "- Regions: ", nRegions, "\n",
    "- Variables (config): ", nCfgVars, "\n",
    "- Variables (data): ", nVariables, "\n",
    "- Variables (hist): ", nVariables, "\n",
    "- # of config vars in scen data: ", nMatchedScen, "\n",
    "- # of config vars in hist data: ", nMatchedHist, "\n"
  ))

  # warn if nothing matches
  if (nMatchedScen == 0 | nMatchedHist == 0)  {
    warning(paste(
      "No variables in data match variables in config.\n",
      "Validation will return empty results.\n\n",
      "Example data variables:\n",
      paste(head(scenVars, 5), collapse = ", "), "\n\n",
      "Example config variables:\n",
      paste(head(cfgVars, 5), collapse = ", ")
    ))
  }
}

#' Warn if reference data required by the config is missing from input data
#'
#' Compares the reference models of all historical checks in the config
#' against the models found in the reference data provided by the user
#' (scenario "historical") and warns about missing ones. If reference data
#' files shipped with the package contain the missing models, they are
#' suggested.
#'
#' @param cfg processed config as used in ``validateScenarios()``
#' @param hist historical/reference data as used in ``validateScenarios()``
checkRefData <- function(cfg, hist) {
  histRows <- cfg[!is.na(cfg$ref_scenario) & cfg$ref_scenario == "historical" &
                    !is.na(cfg$ref_model), ]
  if (nrow(histRows) == 0) return(invisible(NULL))

  # extract model names from ref_model entries, dropping a possible mode
  # such as "range(...)" or "mean(...)"
  refModels <- unlist(lapply(histRows$ref_model, function(x) {
    refs <- strsplit(x, split = "\\(|\\)|, |,")[[1]]
    if (grepl("\\(", x)) refs <- refs[-1]
    refs
  }))
  missingModels <- setdiff(unique(refModels), unique(hist$model))
  if (length(missingModels) == 0) return(invisible(NULL))

  # look for reference data shipped with the package with the missing models
  extFiles <- list.files(system.file("extdata", package = "piamValidation"),
                         pattern = "\\.rds$", full.names = TRUE)
  hints <- Filter(function(f) {
    ref <- try(readRDS(f), silent = TRUE)
    is.data.frame(ref) && "model" %in% colnames(ref) &&
      any(missingModels %in% unique(ref$model))
  }, extFiles)

  msg <- paste0(
    "Reference data required by the config was not found in the input data ",
    "(scenario 'historical') for the following model(s):\n",
    paste("-", missingModels, collapse = "\n"),
    "\nThe affected checks can not be performed.")
  if (length(hints) > 0) {
    msg <- paste0(
      msg, "\nReference data shipped with the package contains (some of) ",
      "these models, consider adding to 'dataPath':\n",
      paste("-", hints, collapse = "\n"))
  }
  warning(msg, call. = FALSE, immediate. = TRUE)
}

#' Average 2020 to smoothen Covid shock in historical data
#'
#' Adds a new model for each model in reference data with smoothed 2020 period
#' and name "<model>_smoothed".
#'
#' @param hist reference data as used in ``validateScenarios()``
average_2020 <- function(hist) {
  hist_m <- hist %>%
    filter(period %in% seq(2018, 2022)) %>%
    magclass::as.magpie(spatial = "region")
  hist_m[, 2020, ] <- magclass::dimSums(hist_m, dim = 2)/5
  hist_smoothed <- quitte::as.quitte(hist_m[, , ]) %>%
    filter(period == 2020) %>%
    mutate(model = paste0(model, "_smoothed"))

  hist <- rbind(hist, hist_smoothed)
  return(hist)
  }

