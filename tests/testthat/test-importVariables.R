test_that("importScenarioData keeps only requested variables", {
  df <- tibble::tibble(
    model = "REMIND", scenario = "Test", region = "World", unit = "EJ/yr",
    variable = c("FE", "FE|+|Electricity", "FE|Electricity", "FE|Heat",
                 "FE|Electricity|Buildings", "PE"),
    period = 2010, value = 1:6
  )

  # exact match, plus notation is removed before matching
  d <- importScenarioData(df, variables = c("FE", "FE|Electricity"))
  expect_setequal(as.character(unique(d$variable)), c("FE", "FE|Electricity"))
  # "FE|+|Electricity" and "FE|Electricity" are merged into one level
  expect_equal(nrow(d), 3)
  expect_setequal(levels(d$variable), c("FE", "FE|Electricity"))

  # wildcards as in the config
  d <- importScenarioData(df, variables = "FE|*")
  expect_setequal(as.character(unique(d$variable)),
                  c("FE|Electricity", "FE|Heat"))
  d <- importScenarioData(df, variables = "FE|**")
  expect_setequal(as.character(unique(d$variable)),
                  c("FE|Electricity", "FE|Heat", "FE|Electricity|Buildings"))

  # no filter keeps everything
  d <- importScenarioData(df)
  expect_setequal(as.character(unique(d$variable)),
                  c("FE", "FE|Electricity", "FE|Heat",
                    "FE|Electricity|Buildings", "PE"))
})

test_that("variable filter is applied while reading files", {
  mif <- file.path(tempdir(), "filter_test.mif")
  writeLines(c(
    "Model;Scenario;Region;Variable;Unit;2005;2010;",
    "REMIND;Test;World;FE;EJ/yr;10;11;",
    "REMIND;Test;World;FE|+|Electricity;EJ/yr;1;2;",
    "REMIND;Test;World;PE;EJ/yr;20;22;"
  ), mif)

  d <- importScenarioData(mif, variables = c("FE|Electricity", "PE"))
  expect_setequal(as.character(unique(d$variable)), c("FE|Electricity", "PE"))
  expect_equal(nrow(d), 4)

  # result equals the unfiltered import restricted to the same variables
  full <- importScenarioData(mif)
  full <- full[full$variable %in% c("FE|Electricity", "PE"), ]
  full$variable <- droplevels(full$variable)
  expect_equal(d, full, ignore_attr = TRUE)
  unlink(mif)
})
