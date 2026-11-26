#' Check variable consistency
#'
#' Test whether unit of on row of config and data for this variable match.
#'
#' @importFrom dplyr filter
#'
#' @param data scenario or reference data for one variable
#' @param cfgRow one row of a config file containing the same variable as the
#'        data object
#'
checkUnits <- function(data, cfgRow) {
  # no unit in config or empty data object: skip the check
  if (nrow(data) != 0 & !is.na(cfgRow$unit)) {
    # identify data type
    dataType <-
      if(cfgRow$ref_scenario %in% "historical") "reference" else "scenario"

    units <- as.character(unique(data$unit))
    for (n in 1:length(units)) {
      if (!areUnitsIdentical(cfgRow$unit, units[n])) {
        warning(paste0(
          "Non-matching units in config and ", dataType," data found.\n",
          "variable: ", cfgRow$variable, "\n",
          "config unit: ", cfgRow$unit, "\n",
          dataType, " unit: ", units[n]), "\n")
        # in case of the presence of non-matching units:
        # filter data for correct unit as it might also available, if not this
        # will result in an empty data object being returned
        data <- filter(data, data$unit %in% cfgRow$unit)
      }
    }
  }
  return(data)
}

# Check whether units are identical, allowing for different spellings of the
# same unit. Copied from piamInterfaces::areUnitsIdentical() (LGPL-3, author:
# Oliver Richters) to avoid the dependency; keep both lists in sync if
# piamInterfaces adds new equivalences.
# vec1, vec2: units to be compared elementwise; vec2 defaults to vec1[1]
# returns a logical vector
areUnitsIdentical <- function(vec1, vec2 = NULL) {
  if (is.null(vec2)) vec2 <- utils::head(vec1, n = 1)
  # abbreviations that are used in the units
  abbreviations <- list(
    "G" = "billion ?|bn ?",
    "M" = "million ?|Million ?|mio ?",
    "US" = "US\\$|USD_|USD|US_",
    "US05" = "US2005",
    "US10" = "US2010",
    "EUR" = "EUR_",
    "yr" = "year",
    "CO2" = "CO2e|CO2eq|CO2-equiv",
    "CF4" = "CF4-equiv",
    "%" = "Percentage|Percent|percent",
    "cap" = "capita",
    "/" = " per "
  )
  # units that have the same meaning, just a different spelling
  identicalUnits <- list(
    c("\u00B0C", "\u00C2\u00B0C", "K"),
    c("kcal/cap/day", "kcal/cap/d"),
    c("ktU/yr", "kt U/yr"),
    c("km\u00B3", "km3"),
    c("mio", "mio people"),
    c("Mt Nr/yr", "Tg N/yr"),
    c("Mt NO2/yr", "Mt NOx/yr"),
    c("unitless", "", "-", "1", "index", NA),
    c("W/m2", "W/m^2"),
    c("million vehicles", "million veh"),
    c("million", "million people"),
    c("t CO2", "tCO2"),
    # below, units that actually differ, kept for backwards compatibility
    # for 'Energy Service|Residential and Commercial|Floor Space'
    c("bn m2/yr", "bn m2"),
    # for 'Productivity|Yield' and subvariables
    c("t DM/ha", "t DM/ha/yr", "dm t/ha"),
    # for 'Water|Environmental flow violation volume'
    c("km3/yr", "km3"),
    c("mha per yr", "Mha per yr"),
    c("Mha per yr", "million ha/yr"),
    c("m3 per ha", "m3/ha"),
    c("t DM/cap/yr", "tDM/capita/yr")
  )

  .abbreviateUnit <- function(unit) {
    for (abb in names(abbreviations)) {
      unit <- gsub(abbreviations[abb], abb, unit)
    }
    return(unit)
  }

  vec1 <- .abbreviateUnit(vec1)
  vec2 <- .abbreviateUnit(vec2)
  identicalUnits <- lapply(identicalUnits, .abbreviateUnit)

  .areIdentical <- function(x, y) {
    # literally identical or both in the same element of identicalUnits
    isTRUE(x == y) ||
      any(unlist(lapply(identicalUnits, function(units) all(c(x, y) %in% units))))
  }

  return(unname(unlist(Map(Vectorize(.areIdentical), vec1, vec2))))
}
