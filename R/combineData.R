#' Combine scenario and reference data with thresholds
#'
#' for one row of cfg: filter and merge relevant scenario data with cfg
#' results in one df that contains scenario data, reference data and thresholds
#'
#' @param scenData scenario data for one variable
#' @param cfgRow one row of a config file
#' @param histData reference data
#'
#' @importFrom dplyr filter select mutate summarise group_by %>%
#' @importFrom piamInterfaces areUnitsIdentical
#'
combineData <- function(scenData, cfgRow, histData = NULL) {

  # shorten as it will be used a lot
  c <- cfgRow

  # full dimensions and important slices
  all_mod <- unique(scenData$model)
  all_sce <- unique(scenData$scenario)
  all_reg <- unique(scenData$region)
  all_per <- unique(scenData$period)
  all_per <- all_per[all_per <= 2100]
  hist_per <- c(2005, 2010, 2015, 2020)

  # create filters ####
  # check whether regions, periods, scenarios are specified, else use all
  mod <- if (is.na(c$model))    all_mod else
    strsplit(c$model, split = ", |, ")[[1]]
  sce <- if (is.na(c$scenario)) all_sce else
    strsplit(c$scenario, split = ", |, ")[[1]]
  reg <- if (is.na(c$region))   all_reg else
    strsplit(c$region, split = ", |,")[[1]]

  # empty "period" field means different years for historic category
  if (c$ref_scenario == "historical" && !is.na(c$ref_scenario == "historical")) {
    per <- if (is.na(c$period))  hist_per else
      strsplit(as.character(c$period), split = ", |,")[[1]]
  } else {
    per <- if (is.na(c$period))  all_per else
      strsplit(as.character(c$period), split = ", |,")[[1]]
  }

  # apply filters ####
  # filter scenario data according to each row in cfg
  d <- scenData %>%
    filter(variable %in% c$variable,
           model    %in% mod,
           scenario %in% sce,
           region   %in% reg,
           period   %in% per)

  # attach cfg information which is independent of category to data slice
  d <- d %>%
    mutate(min_red  = c$min_red,
           min_yel  = c$min_yel,
           max_yel  = c$max_yel,
           max_red  = c$max_red,
           metric   = c$metric,
           critical = c$critical)

  # test whether units of config and scenario data match
  d <- checkUnits(d, c)

  # references ####
  ## historic reference ####
  # depending on category: filter and attach reference values if they are needed
  if (c$ref_scenario == "historical" && !is.na(c$ref_scenario == "historical")) {
    # historic data for relevant variable and dimensions (all sources)

    h <- histData %>%
         filter(variable %in% c$variable,
                region %in% reg,
                period %in% per)
    # test whether any historical data is available for this variable
    if (nrow(h) == 0) {
      cat(paste0("No reference data for variable ", c$variable, " found.\n"))
    }

    # test whether units of config and reference data match
    h <- checkUnits(h, c)

    # in case no model is specified, write all available models in ref_model
    if (is.na(c$ref_model)) {
      c$ref_model <- paste(unique(h$model), collapse = ", ")
    }

    h <- refineRefData(h, c, "ref_model") %>%
      select(-scenario)

    # merge with historical data adds columns ref_value_min/max and ref_model
    # from h to d
    if (nrow(h) == 0) {
      stop(paste0("No reference data of model ", c$ref_model ,
                  " found for variable ", c$variable, ".\n"))
    } else {
      df <- merge(d, h) %>%
      # add columns which are not used in this category
        mutate(ref_period = as.character(NA),
               ref_scenario = "historical")
    }


  # filter and attach reference values if they are needed; scenario data
  } else {

    # no reference values needed for these metrics, fill NA
    if (c$metric %in% c("absolute", "growthrate")) {
      df <- d %>%
        mutate(ref_value_min = as.numeric(NA),
               ref_value_max = as.numeric(NA),
               ref_model     = as.character(NA),
               ref_scenario  = as.character(NA),
               ref_period    = as.character(NA))

    ## scenario data reference ####
    # get reference values for these metrics
    } else if (c$metric %in% c("relative", "difference")) {

      # if a reference model should be used, same scenario, same period
      if (!is.na(c$ref_model)) {

        # pre-filter reference data according to scenario data filters
        ref_data <- scenData %>%
          filter(variable %in% c$variable,
                 scenario %in% sce,
                 region   %in% reg,
                 period   %in% per)

        # evaluate references specified in config file
        ref_data <- refineRefData(ref_data, c, "ref_model")

      # if a reference scenario should be used, same period, same model
      } else if (!is.na(c$ref_scenario)) {

        # pre-filter reference data according to scenario data filters
        ref_data <- scenData %>%
          filter(variable %in% c$variable,
                 model    %in% mod,
                 region   %in% reg,
                 period   %in% per)

        # evaluate references specified in config file
        ref_data <- refineRefData(ref_data, c, "ref_scenario")

      # if a reference period should be used, same scenario, same model
      } else if (!is.na(c$ref_period)) {

        # pre-filter reference data according to scenario data filters
        ref_data <- scenData %>%
          filter(variable %in% c$variable,
                 model    %in% mod,
                 scenario %in% sce,
                 region   %in% reg)

        # evaluate references specified in config file
        ref_data <- refineRefData(ref_data, c, "ref_period")

      } else {
        stop("No reference selected. Please choose what to compare to in one of
        the 'ref_' columns of the validation config.\n")
      }

      # this will work for all reference types as ref_data columns are adjusted
      # depending on which reference type is used
      df <- merge(d, ref_data)

    } else {
      warning("'metric' must be either 'absolute',
              'relative', 'difference' or 'growthrate'.")
    }
  }
  return(df)
}

#' evaluate the content of the "ref_<type>" column and filter
#' reference data accordingly.
#' cases:
#'  - mode chosen
#'    - range
#'    - mean
#'  - no mode chosen
#'    - use mean if multiple references
#'
#' returns df without variable, unit and <type> columns (see below)
#' returns df with ref_value_min/max, ref_model, ref_scenario, ref_period
#'
#' @param ref_data pre-filtered reference data
#' @param cfgRow row of validation config used for this data slice
#' @param ref_type historical, model, scenario, period
#'
refineRefData <- function(ref_data, cfgRow, ref_type = "ref_model") {
  # objects to help summarise data for different reference types
  type <- gsub("ref_", "", ref_type)
  group_columns <- c("model", "scenario", "period", "region")

  # a bracket shows that a mode of how to use the references is used
  if (grepl("\\(", cfgRow[, ref_type][[1]])) {
    # mode chosen ####
    # split string into mode ([1]) and references ([2:end])
    refs <- strsplit(cfgRow[, ref_type][[1]], split = "\\(|\\)|, |,")[[1]]

    # filter out data of references specified in cfgRow
    ref_data <- ref_data %>%
      filter(!!as.symbol(type) %in% refs[2:length(refs)]) %>%
      select(-all_of(type))

    if (refs[1] == "range") {
      # range ####
      # min/max for each reference point
      ref_data <- ref_data %>%
        # take the range of values of the one dim that is the reference
        group_by(across(setdiff(group_columns, type))) %>%
        summarise(ref_value_min = min(value, na.rm = TRUE),
                  ref_value_max = max(value, na.rm = TRUE),
                  .groups = "drop")
    } else if (refs[1] == "mean") {
      # mean ####
      ref_data <- ref_data %>%
        # take the mean over the one dim that is the reference
        group_by(across(setdiff(group_columns, type))) %>%
        summarise(ref_value_min = mean(value, na.rm = TRUE),
                  ref_value_max = ref_value_min,
                  .groups = "drop")
    } else {
      stop("Only 'mean' and 'range' are allowed as mode when using
                  multiple references.")
    }

  } else {
    # no mode chosen ####
    # reference is not empty but no mode is specified -> use mean by default
    # filter out data of one or multiple references specified in cfgRow
    refs <- strsplit(as.character(cfgRow[, ref_type][[1]]), split = ", |,")[[1]]

    # if comma separated list of ref_models, use mean of all (min = max)
    # will work for a single reference as well
    ref_data <- ref_data %>%
      filter(!!as.symbol(type) %in% refs) %>%

      # grouping across all dimensions except the one chosen as reference
      group_by(across(setdiff(group_columns, type))) %>%
      summarise(ref_value_min = mean(value, na.rm = TRUE),
                ref_value_max = ref_value_min,
                .groups = "drop")
  }

  # attach variable and ref_model columns from config
  ref_data <- ref_data %>%
    mutate(ref_model    = cfgRow$ref_model,
           ref_scenario = cfgRow$ref_scenario,
           ref_period   = as.character(cfgRow$ref_period))

  return(ref_data)
}
