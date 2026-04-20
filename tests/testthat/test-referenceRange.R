test_that("references can be used as range", {

  # TODO: find out how to solve the error when running
  # validateScenarios(data, config)

  config <- suppressMessages(getConfig(
    testthat::test_path("testdata", "validationConfig_testReferenceRange.csv")))

  data <- importScenarioData(
    testthat::test_path("testdata", "data_testUseCases.rds"))
  # add second historical model to test data, set 200 higher to have range
  data <-
    rbind(data,
          data[17:20, ] %>%
            mutate(model = "EDGAR", value = value + 200)
    )

  # evaluate whether all validation has been performed as expected
  # historical checks ####
  # row 1 to 8: comparison to historical reference data
  df1 <- validateScenarios(data, config[1, ])
  df2 <- validateScenarios(data, config[2, ])

  # default of multiple references: calculate arithmetic mean
  ref_val_CEDS  <- data[data$model == "CEDS"  & data$period == 2010, "value"]
  ref_val_EDGAR <- data[data$model == "EDGAR" & data$period == 2010, "value"]
  ref_val = (ref_val_CEDS + ref_val_EDGAR) / 2

  # row 1: no ref model specified, expected to use mean of all available models
  expect_equal(df1$ref_value_min[1], ref_val[[1]])
  expect_equal(df1$ref_value_max[1], ref_val[[1]])
  # row 2: both models comma-separated, mean of all mentioned models expected
  expect_equal(df2$ref_value_min[1], ref_val[[1]])
  expect_equal(df2$ref_value_max[1], ref_val[[1]])

  # only one source given
  df3 <- validateScenarios(data, config[3, ])
  expect_equal(df3$ref_value_min[1], ref_val_CEDS[[1]])
  expect_equal(df3$ref_value_max[1], ref_val_CEDS[[1]])

  # expect mean if references are given as "mean(ref1, ref2)"
  df4 <- validateScenarios(data, config[4, ])
  expect_equal(df4$ref_value_min[1], ref_val[[1]])
  expect_equal(df4$ref_value_max[1], ref_val[[1]])

  # use range of references:
  # - min thresholds compare to CEDS as it has the lower value
  # - max thresholds compare to EDGAR as it has higher values
  df5 <- validateScenarios(data, config[5, ])
  expect_equal(df5$ref_value_min[1], ref_val_CEDS[[1]])
  expect_equal(df5$ref_value_max[1], ref_val_EDGAR[[1]])

  # TODO: 6,7,8 for difference instead of relative historic checks

  # tests using scenario data as reference
  # model comparison ####
  ref_val_model1 <-
    data[data$model == "Model1" & data$period %in% c(2010, 2015), "value"]
  ref_val_model2 <-
    data[data$model == "Model2" & data$period %in% c(2010, 2015), "value"]
  ref_val_model <- (ref_val_model1 + ref_val_model2)/2

  # comparison of Model2 to Model1 only
  df9 <- validateScenarios(data, config[9, ])
  expect_equal(df9$ref_value_min, ref_val_model1[[1]])
  expect_equal(df9$ref_value_max, ref_val_model1[[1]])

  # comparison of Model1 to Model1 and Model2 without specifying mode
  df10 <- validateScenarios(data, config[10, ])
  expect_equal(df10$ref_value_min, ref_val_model[[1]])
  expect_equal(df10$ref_value_max, ref_val_model[[1]])

  # comparing both models against their mean by specifying mode
  df11 <- validateScenarios(data, config[11, ])
  expect_equal(df11$ref_value_min, rep(ref_val_model[[1]], each = 2))
  expect_equal(df11$ref_value_max, rep(ref_val_model[[1]], each = 2))

  # comparing both model against their range by specifying mode
  df12 <- validateScenarios(data, config[12, ])
  # element wise min and max of the two models
  ref_model_min <- rep(pmin(ref_val_model1[[1]], ref_val_model2[[1]]), each = 2)
  ref_model_max <- rep(pmax(ref_val_model1[[1]], ref_val_model2[[1]]), each = 2)
  expect_equal(df12$ref_value_min, ref_model_min)
  expect_equal(df12$ref_value_max, ref_model_max)

  # scenario comparison ####
  ref_val_scen1 <-
    data[data$scenario == "Scen1" & data$period %in% c(2010, 2015), "value"]
  ref_val_scen2 <-
    data[data$scenario == "Scen2" & data$period %in% c(2010, 2015), "value"]
  ref_val_scen <- (ref_val_scen1 + ref_val_scen2)/2

  # comparing both scenarios against Scen1 without specifying mode
  df13 <- validateScenarios(data, config[13, ])
  expect_equal(df13$ref_value_min, rep(ref_val_scen1[[1]], each = 2))
  expect_equal(df13$ref_value_max, rep(ref_val_scen1[[1]], each = 2))

  # comparing scenario 1 against Scen1 and Scen2 without specifying mode
  df14 <- validateScenarios(data, config[14, ])
  expect_equal(df14$ref_value_min, ref_val_scen[[1]])
  expect_equal(df14$ref_value_max, ref_val_scen[[1]])

  # comparing both scenarios against their mean by specifying mode
  df15 <- validateScenarios(data, config[15, ])
  expect_equal(df15$ref_value_min, rep(ref_val_scen[[1]], each = 2))
  expect_equal(df15$ref_value_max, rep(ref_val_scen[[1]], each = 2))

  # comparing both scenarios against their range by specifying mode
  df16 <- validateScenarios(data, config[16, ])
  # element wise min and max of the two scenarios
  ref_scen_min <- rep(pmin(ref_val_scen1[[1]], ref_val_scen2[[1]]), each = 2)
  ref_scen_max <- rep(pmax(ref_val_scen1[[1]], ref_val_scen2[[1]]), each = 2)
  expect_equal(df16$ref_value_min, ref_scen_min)
  expect_equal(df16$ref_value_max, ref_scen_max)

  # period comparison ####
  ref_val_2010 <-
    data[data$period == 2010 & data$scenario != "historical", "value"]
  ref_val_2015 <-
    data[data$period == 2015 & data$scenario != "historical", "value"]
  ref_val_period <- (ref_val_2010 + ref_val_2015)/2

  # compare 2020 to 2010 without specifying mode
  df17 <- validateScenarios(data, config[17, ])
  expect_equal(df17$ref_value_min, ref_val_2010[[1]])
  expect_equal(df17$ref_value_max, ref_val_2010[[1]])

  # compare 2010 and 2015 to themselves without specifying mode
  df18 <- validateScenarios(data, config[18, ])
  expect_equal(df18$ref_value_min, rep(ref_val_period[[1]], each = 2))
  expect_equal(df18$ref_value_max, rep(ref_val_period[[1]], each = 2))

  # compare 2010 to 2010 and 2015, using mean explicitly
  df19 <- validateScenarios(data, config[19, ])
  expect_equal(df19$ref_value_min, ref_val_period[[1]])

  # compare all 4 periods to range 2010, 2015
  df20 <- validateScenarios(data, config[20, ])
  # element wise min and max of the two periods
  ref_per_min <- rep(pmin(ref_val_2010[[1]], ref_val_2015[[1]]), each = 4)
  ref_per_max <- rep(pmax(ref_val_2010[[1]], ref_val_2015[[1]]), each = 4)
  expect_equal(df20$ref_value_min, ref_per_min)
  expect_equal(df20$ref_value_max, ref_per_max)
})
