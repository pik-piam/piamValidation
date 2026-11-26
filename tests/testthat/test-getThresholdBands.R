test_that("threshold bands are computed from validateScenarios output", {

  cfg <- testthat::test_path("testdata", "validationConfig_testSparseThresholds.csv")
  data <- testthat::test_path("testdata", "REMIND_testdata.rds")

  valiData <- suppressMessages(validateScenarios(data, cfg))
  bands <- getThresholdBands(valiData)

  expect_equal(colnames(bands),
               c("variable", "unit", "region", "period",
                 "min_red", "min_yel", "max_yel", "max_red"))

  # config contains one absolute check: FE in 2005 with thresholds 0;1;50;100
  expect_equal(nrow(bands), 1)
  expect_equal(as.character(bands$variable), "FE")
  expect_equal(bands$min_red, 0)
  expect_equal(bands$min_yel, 1)
  expect_equal(bands$max_yel, 50)
  expect_equal(bands$max_red, 100)
})

test_that("relative and difference thresholds are converted to absolute bands", {

  valiData_rel <- data.frame(
    model = "REMIND", scenario = "testScen",
    variable = "FE", unit = "EJ/yr", region = "World", period = 2020,
    metric = "relative",
    min_red = -0.5, min_yel = -0.2, max_yel = 0.2, max_red = 0.5,
    ref_value_min = 10, ref_value_max = 10
  )
  bands_rel <- getThresholdBands(valiData_rel)
  expect_equal(bands_rel$min_red, 5)
  expect_equal(bands_rel$min_yel, 8)
  expect_equal(bands_rel$max_yel, 12)
  expect_equal(bands_rel$max_red, 15)

  valiData_dif <- data.frame(
    model = "REMIND", scenario = "testScen",
    variable = "FE", unit = "EJ/yr", region = "World", period = 2020,
    metric = "difference",
    min_red = -4, min_yel = -2, max_yel = 2, max_red = 4,
    ref_value_min = 10, ref_value_max = 10
  )
  bands_dif <- getThresholdBands(valiData_dif)
  expect_equal(bands_dif$min_red, 6)
  expect_equal(bands_dif$min_yel, 8)
  expect_equal(bands_dif$max_yel, 12)
  expect_equal(bands_dif$max_red, 14)
})

test_that("identical thresholds of multiple scenarios collapse into one band", {

  valiData <- data.frame(
    model = "REMIND", scenario = c("scen1", "scen2"),
    variable = "FE", unit = "EJ/yr", region = "World", period = 2020,
    metric = "absolute",
    min_red = 0, min_yel = 1, max_yel = 50, max_red = 100,
    ref_value_min = NA, ref_value_max = NA
  )
  bands <- getThresholdBands(valiData)
  expect_equal(nrow(bands), 1)
})

test_that("multiple threshold sets for the same data point give a warning", {

  valiData <- data.frame(
    model = "REMIND", scenario = "testScen",
    variable = "FE", unit = "EJ/yr", region = "World", period = 2020,
    metric = c("absolute", "relative"),
    min_red = c(0, -0.5), min_yel = c(1, -0.2),
    max_yel = c(50, 0.2), max_red = c(100, 0.5),
    ref_value_min = c(NA, 10), ref_value_max = c(NA, 10)
  )
  expect_warning(getThresholdBands(valiData), "more than one set of thresholds")
  expect_no_warning(getThresholdBands(valiData, warnMultiple = FALSE))
  expect_equal(nrow(suppressWarnings(getThresholdBands(valiData))), 2)
})
