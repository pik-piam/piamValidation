#' import IAM data for validation
#'
#' @importFrom dplyr filter select mutate %>%
#' @importFrom readxl read_excel excel_sheets
#' @importFrom utils read.csv2
#'
#' @param scenarioPath one or multiple paths to .mif, .csv, .rds or .xlsx file(s)
#'        or a data.frame containing scenario data in IAM format
#' @param variables optional character vector of variable names (may contain
#'        "*" wildcards as in the config); if given, all other variables are
#'        dropped while reading so that only the data needed for the
#'        validation is held in memory
importScenarioData <- function(scenarioPath, variables = NULL) {
  # reduction applied to every chunk while reading .mif/.csv, and to the full
  # object for .rds/.xlsx and data frames
  reduce <- function(d) {
    d %>%
      filter(period >= 1990) %>%
      mutate(variable = deletePlusLevels(variable)) %>%
      filterVariables(variables)
  }

  if (is.character(scenarioPath)) {
    # read.quitte handles .mif, .csv, .rds and .xlsx (as as.quitte does) and
    # applies filter.function per 200k-line chunk for text files
    data <- quitte::read.quitte(scenarioPath, sep = NULL,
                                filter.function = reduce) %>%
      quitte::as.quitte(na.rm = TRUE)
  } else {
    # data object given: avoid re-converting if it already is a quitte object
    data <- if (quitte::is.quitte(scenarioPath)) scenarioPath else
      quitte::as.quitte(scenarioPath, na.rm = TRUE)
    data <- reduce(data)
  }

  # change ordering of factors, global elements first
  new_order <- unique(intersect(c("World", "GLO",
                                  levels(data$region)), levels(data$region)))
  data$region <- factor(data$region, levels = new_order)

  return(data)
}

# remove the "+" notation from variable names; operate on the factor levels
# rather than on the full column as the latter creates a character vector with
# one entry per row of the data
deletePlusLevels <- function(variable) {
  if (is.factor(variable)) {
    # `levels<-` merges levels that become identical (e.g. "FE|+|Elec" and
    # "FE|Elec"), so no re-factoring is needed
    levels(variable) <- piamutils::deletePlus(levels(variable))
    variable
  } else {
    factor(piamutils::deletePlus(variable))
  }
}

# keep only variables that are needed for the validation; exact names are
# matched directly, names containing "*" are matched as in expandVariables()
filterVariables <- function(data, variables) {
  if (is.null(variables)) return(data)

  # works for factor and character columns alike
  allVars <- if (is.factor(data$variable)) levels(data$variable) else
    unique(as.character(data$variable))
  isWild <- grepl("*", variables, fixed = TRUE)
  keep <- allVars %in% variables[!isWild]
  for (v in variables[isWild]) {
    keep <- keep | grepl(variableToRegex(v), allVars)
  }

  data <- filter(data, variable %in% allVars[keep])
  if (is.factor(data$variable)) data$variable <- droplevels(data$variable)
  return(data)
}

# convert a variable name from the config into a regular expression matching
# the full variable name
# * matches everything until the next |, while ** matches including |
variableToRegex <- function(variable) {
  # escape "|"
  vargrep <- gsub("|", "\\|", variable, fixed = TRUE)
  # convert * into "everything except |"
  vargrep <- gsub("*", "[^\\|]*", vargrep, fixed = TRUE)
  # convert what was ** back to .*
  vargrep <- gsub("[^\\|]*[^\\|]*", ".*", vargrep, fixed = TRUE)
  # make sure you match the full variable, not just a part
  paste0("^", vargrep, "$")
}

#' import a config shipped with the package
#'
#' get a validation config file either from "inst/config" (.csv) or by providing
#' a full path (.csv or .xlsx) or a tibble with the necessary columns
#' see README or vignette for rules on how to fill the config
#' @param config name of validation config from "inst/config"
#'
#' @export
getConfig <- function(config) {
  # config can be a data object...
  if (tibble::is_tibble(config)) {
    cfg <- config
  # ...or a string
  } else if (is.character(config)) {
    # look for config in package
    path <- system.file(paste0("config/validationConfig_", config, ".csv"),
                        package = "piamValidation")
    # if a full path is given instead of a config in inst/config
    if (!file.exists(path) && file.exists(normalizePath(config))) {
      path <- normalizePath(config)
    }
    if (path == "") stop("Config not found, please provide either full path to a
    config file or select a config from 'inst/config' by choosing its
    name ('validationConfig_<name>.csv').\n")

    # config can be .xlsx or .csv, use "config" sheet in .xlsx if available
    if (grepl("\\.xlsx$", path)) {
      cfg <- read_excel(
        path = path,
        sheet = if ("config" %in% excel_sheets(path)) "config" else 1
        )
      cfg <- filter(cfg, ! grepl("^#", cfg[[1]]))
    } else {
      # only support ";" as separator for config as "," might be used in cells
      cfg <- tibble::as_tibble(
        read.csv2(path, na.strings = c("", "NA"), comment.char = "#"))
    }
    message("loading config file: ", path, "\n")

  } else {
    stop("Please specify config either as character (file path or name)
         or as tibble.")
  }

  # remove empty (all NA) rows
  cfg <- cfg[rowSums(is.na(cfg)) != ncol(cfg), ]

  # remove rows without variables
  cfg <- cfg[!is.na(cfg$variable), ]

  # convert "%" thresholds to decimals
  cfg <- cfg %>%
    mutate(min_red = ifelse(grepl("%", min_red),
                            as.numeric(sub("%", "", min_red)) / 100,
                            min_red)) %>%
    mutate(min_yel = ifelse(grepl("%", min_yel),
                            as.numeric(sub("%", "", min_yel)) / 100,
                            min_yel)) %>%
    mutate(max_yel = ifelse(grepl("%", max_yel),
                            as.numeric(sub("%", "", max_yel)) / 100,
                            max_yel)) %>%
    mutate(max_red = ifelse(grepl("%", max_red),
                            as.numeric(sub("%", "", max_red)) / 100,
                            max_red))

  # convert thresholds to numeric
  cfg <- cfg %>%
    mutate(min_red = as.numeric(min_red),
           min_yel = as.numeric(min_yel),
           max_yel = as.numeric(max_yel),
           max_red = as.numeric(max_red)
           )
  return(cfg)
}

# fill empty and NA threshold columns with Infinity for easier evaluation
fillInf <- function(cfg) {
  cfg <- cfg %>%
    mutate(min_red = ifelse(is.na(min_red) | min_red == "NA", -Inf, min_red),
           min_yel = ifelse(is.na(min_yel) | min_yel == "NA", -Inf, min_yel),
           max_yel = ifelse(is.na(max_yel) | max_yel == "NA",  Inf, max_yel),
           max_red = ifelse(is.na(max_red) | max_red == "NA",  Inf, max_red)
           )

  return(cfg)
}


# replace period ranges using ":" with comma-separated list of years
expandPeriods <- function(cfg, data) {
  per_expand_idx <- grep("\\-", cfg$period)
  all_per <- unique(data$period)

  # iterate through rows with ":"
  for (i in per_expand_idx) {
    # check format and compatibility of data and ref periods
    if (nchar(cfg[i, "period"]) != 9) {
      stop("Invalid range detected. Make sure to enter years as YYYY-YYYY.\n")
    } else {
      # find scenario-years that are withing indicated range
      start <- substr(cfg[i, "period"], 1, 4)
      if (start < min(all_per)) {
        warning("Selected period starts earlier than scenario data.\n")
        }
      stop <- substr(cfg[i, "period"], 6, 9)
      if (stop > max(all_per)) {
        warning("Selected period ends later than scenario data.\n")
      }
      selected_per <- all_per[all_per >= start & all_per <= stop]

      # overwrite ":" notation with list
      cfg[i, "period"] <- paste0(selected_per, collapse = ", ")
      }
  }
  return(cfg)
}


# takes config entries specifying a set of variables via "*" and expands it so
# that every variable corresponds to one row in cfg
# * matches everything until the next |, while ** matches including |
expandVariables <- function(cfg, data) {
  # create the expanded config, starting with the not-to-expand rows,
  # then appending the rows with expanded variable names
  var_expand <- cfg[grepl("\\*", cfg$variable), ]
  cfg_new <- dplyr::anti_join(cfg, var_expand, by = colnames(cfg))

  # TODO: expand variables into exact place in config as order can be important
  if (length(var_expand > 0)) {
    all_vars <- unique(data$variable)
    for (i in seq(nrow(var_expand))) {
      vargrep <- variableToRegex(var_expand$variable[i])
      selected_vars <- all_vars[grepl(vargrep, all_vars)]
      message(var_expand$variable[i], " was expanded into ",
              length(selected_vars), " sub-variables.")

      # take the original row for the current set of variables and repeat it
      # once for each sub-variable, overwrite with sub-variable names
      c <- var_expand[i, ] %>%
        dplyr::slice(rep(1, each = length(selected_vars)))
      c$variable <- selected_vars
      cfg_new <- rbind(c, cfg_new)
    }
  }
  if (nrow(cfg) == 0) {
    stop("Empty config returned, check if variable names of config are
         consistent with those in data.")
  }
  return(cfg_new)
}
