#' List available configs
#'
#' List all validation configuration files that are delivered with the package
#' and can be directly imported with ``getConfig()`` or used in
#' ``validateScenarios()`` and ``validationReport()``.
#'
#' @export
listConfigs <- function() {

  configs <- list.files(system.file("config/", package = "piamValidation"))
  configs <- gsub("validationConfig_|.csv", "", configs)

  cat("Available configuration files\n")
  paste(configs)
}
