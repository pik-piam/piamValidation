#' Import SCI criteria and convert to config
#'
#' Use installed "scenario-evaluation-criteria" package to convert threshold
#' criteria from Scenario Compass release to a piamValidation config and a
#' reference data .rds file
#'
#' @param version criteria version that should be converted to config
#' @importFrom dplyr filter mutate select
#'
#' @export
convertSCIcriteria <- function(version = "2026.8.3") {

  # check if right version is installed
  installed <- utils::packageVersion("scenarioevaluationcriteria")
  if (installed != version) {
    stop(paste0("Installed version of scenario-evaluation-criteria (", installed,
    ") does not match selected criteria version: ", version))
  }

  criteria <- scenarioevaluationcriteria::load_criteria(
    components = c("criteria-thresholds", "criteria-types", "reference-data"))

  # criteria[["criteria-types"]] defines possible outcomes of checks
  # only failed historical tests currently lead to exclusion of scenario
  # -> needs to be updated manually if it changes
  #
  # Historical Vetting
  # - ok = green
  # - failed = blue/red
  # - insufficient reporting = grey
  #
  # Feasibility Concern
  # - ok = green
  # - medium = cyan/yellow
  # - high = blue/red
  # - not assigned = grey
  #
  # Sustainability Concern
  # - ok = green
  # - medium = cyan/yellow
  # - high = blue/red
  # - not assigned = grey

  thresh <- criteria[["criteria-thresholds"]]
  # separate by type
  hist <- thresh[grepl("Historical Vetting", thresh$criterion), ]
  feas <- thresh[grepl("Feasibility Concern", thresh$criterion), ]
  sust <- thresh[grepl("Sustainability Concern", thresh$criterion), ]

  if (nrow(rbind(hist, feas, sust)) != nrow(thresh)) {
    stop("Criterion separation failed, rows don't add up!")
  }

  # convert config ####
  # convert each type to piamValidation format
  ## historical ####
  hist <- hist %>%
    mutate(period = .data$year,
           ref_model = .data$reference_data,
           min_red = .data$lower - 1,  # SCI uses different conventions for relative checks
           max_red = .data$upper - 1,

           metric = "relative",
           critical = "yes",
           model = NA,
           scenario = NA,
           min_yel = NA,
           max_yel = NA,
           ref_scenario = "historical",
           ref_period = NA,
           unit = NA)  %>%
    select(-c(year, reference_data, lower, upper, criterion, evaluation_outcome))

  ## feasibility ####
  feas_m <- feas %>%
    filter(evaluation_outcome == "medium") %>%
    mutate(min_yel = .data$lower,
           max_yel = .data$upper) %>%
    select(-c(lower, upper, evaluation_outcome, criterion, reference_data))
  feas_h <- feas %>%
    filter(evaluation_outcome == "high") %>%
    mutate(min_red = .data$lower,
           max_red = .data$upper) %>%
    select(-c(lower, upper, evaluation_outcome, criterion, reference_data))

  feas <- merge(feas_m, feas_h,
        by = c("variable", "unit", "region", "year"),
        all = TRUE) %>%  # some variable only have medium concerns

    mutate(period = .data$year,
           metric = "absolute",
           critical = "feasibility",  # failed checks do not cause the scenario to be disqualified
           model = NA,
           scenario = NA,
           ref_model = NA,
           ref_scenario = NA,
           ref_period = NA)  %>%
    select(-year)

  # use only better variable, as we have it in REMIND
  feas[feas$variable == "Carbon Capture|Geological Storage, Carbon Capture", "variable"] <-
    "Carbon Capture|Geological Storage"

  ## sustainability ####
  sust_m <- sust %>%
    filter(evaluation_outcome == "medium") %>%
    mutate(min_yel = .data$lower,
           max_yel = .data$upper) %>%
    select(-c(lower, upper, evaluation_outcome, criterion, reference_data))
  sust_h <- sust %>%
    filter(evaluation_outcome == "high") %>%
    mutate(min_red = .data$lower,
           max_red = .data$upper) %>%
    select(-c(lower, upper, evaluation_outcome, criterion, reference_data))

  sust <- merge(sust_m, sust_h,
                by = c("variable", "unit", "region", "year")) %>%

    mutate(period = .data$year,
           metric = "absolute",
           critical = "sustainability",  # failed checks do not cause the scenario to be disqualified
           model = NA,
           scenario = NA,
           ref_model = NA,
           ref_scenario = NA,
           ref_period = NA)  %>%
    select(-year)

  # manually rename row with "cumulative period" -> should be handled via variable name instead
  sust[sust$period == "cumulative[2020-2100]", "variable"] = paste0(sust[sust$period == "cumulative[2020-2100]", "variable"], "|Cumulative")
  sust[sust$period == "cumulative[2020-2100]", "period"] = "2020-2100"

  # combine into one config
  cfg <- rbind(hist, feas, sust, make.row.names = FALSE) %>%
    dplyr::as_tibble()
  cfg <- cfg[c("metric", "critical", "variable", "unit", "model", "scenario",
               "region", "period", "min_red", "min_yel", "max_yel", "max_red",
               "ref_model", "ref_scenario", "ref_period")]

  utils::write.table(
    cfg,
    file = paste0("inst/config/validationConfig_SCI_", version, ".csv"),
    sep = ";", quote = FALSE, row.names = FALSE, col.names = TRUE, na = "")

  # convert reference data ####
  ref <- criteria[["reference-data"]]
  ref <- ref %>%
    mutate(model = .data$reference_data,
           scenario = "historical",
           period = .data$year) %>%
    select(-reference_data, -year)

  saveRDS(ref, file = paste0("inst/extdata/SCI_reference_data_", version, ".rds"))

  # manually convert and save with REMIND variable names
  cfg_rmd <- cfg %>%
    mutate(variable = gsub("Capacity", "Cap", .data$variable),
           variable = gsub("Emissions", "Emi", .data$variable),
           variable = gsub("Primary Energy", "PE", .data$variable),
           variable = gsub("Final Energy", "FE", .data$variable),
           variable = gsub("Carbon Capture\\|Geological Storage", "Carbon Management|Storage", .data$variable),
           variable = gsub("Cumulative", "Cumulated", .data$variable),
           # not part of regular remind runs
           variable = gsub("Food Availability \\[per capita\\]", NA, .data$variable))

  utils::write.table(
    cfg_rmd,
    file = paste0("inst/config/validationConfig_SCI_REMIND_", version, ".csv"),
    sep = ";", quote = FALSE, row.names = FALSE, col.names = TRUE, na = "")

  ref_rmd <- ref %>%
    mutate(variable = gsub("Emissions", "Emi", .data$variable),
           variable = gsub("Primary Energy", "PE", .data$variable),
           variable = gsub("Final Energy", "FE", .data$variable))

  saveRDS(ref_rmd,
          file = paste0("inst/extdata/SCI_reference_data_REMIND_", version, ".rds"))

}
