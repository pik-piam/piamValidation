test_that("expandVariables works", {
  varinconfig <- function(data, varentry, expectedvars) {
    configfile <- file.path(tempdir(), "config.csv")
    configheader <- paste0(
      "category;metric;critical;variable;model;scenario;region;period;",
      "min_red;min_yel;max_yel;max_red;ref_model;ref_scenario;ref_period;")

    configstring <- paste0("scenario;relative;yes;",
                           varentry,
                           ";m;s;r;2005;;;10;100;refmodel1;;;")
    writeLines(c(configheader, configstring), configfile)
    cfg <- suppressMessages(expandVariables(getConfig(configfile), data))
    missing <- setdiff(expectedvars, cfg$variable)
    if (length(missing) > 0) {
      warning("For ", varentry,
              ", the following variables are expected but missing:\n",
              paste0(missing, collapse = ", "))
    }
    expect_length(missing, 0)
    toomuch <- setdiff(cfg$variable, expectedvars)
    if (length(toomuch) > 0) {
      warning("For ", varentry,
              ", the following variables were not expected but matched:\n",
              paste0(toomuch, collapse = ", "))
    }
    expect_true(length(toomuch) == 0)
  }

  data <- list()
  depth1 <- c("Emi|CO", "Emi|BC", "Emi|GHG")
  depth2CO <- c("Emi|CO|Trans", "Emi|CO|Build", "Emi|CO|Indus")
  depth2BC <- c("Emi|BC|Trans", "Emi|BC|Build", "Emi|BC|Indus")
  depth3CO <- c("Emi|CO|Trans|Gas", "Emi|CO|Trans|Oil")
  whatever <- c("Emi", "Final Energy", "CO|Trans")
  data$variable <- c(depth1, depth2CO, depth2BC, depth3CO, whatever)

  # first argument is what is stated in the config, second what should be matched
  varinconfig(data, "Final Energy", "Final Energy")
  varinconfig(data, "Em*", "Emi")
  varinconfig(data, "Emi", "Emi")
  varinconfig(data, "Emi|*", depth1)
  varinconfig(data, "Emi|*|*", c(depth2CO, depth2BC))
  varinconfig(data, "Emi|*|*|*", depth3CO)
  varinconfig(data, "*|CO|*", depth2CO)
  varinconfig(data, "CO|*", "CO|Trans")
  varinconfig(data, "Final Energy|**", NULL)
  varinconfig(data, "Emi|**", c(depth1, depth2CO, depth2BC, depth3CO))
  varinconfig(data, "Em**", c("Emi", depth1, depth2CO, depth2BC, depth3CO))
})
