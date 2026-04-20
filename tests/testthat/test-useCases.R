test_that("all use cases generally work", {
  cfg_path <- testthat::test_path("testdata", "validationConfig_testUseCases.csv")
  dat_path <- testthat::test_path("testdata", "data_testUseCases.rds")

  # full config
  cfg <- suppressMessages(getConfig(cfg_path))

  # test config line by line to avoid sorting problems

  # relative historical comparison ####
  df <- validateScenarios(dat_path, cfg[1, ])
  expect_equal(df$ref_value_min,
               c(28602.36, 28602.36, 28602.36, 28602.36),
               tolerance = 0.1)
  expect_equal(df$check_value_min,
               c(0.1676648, 0.1097486, 0.1173444, 0.1658619),
               tolerance = 0.00001)
  expect_equal(df$check,
               c("yellow", "green", "green", "yellow"))

  # difference historical comparison ####
  df <- validateScenarios(dat_path, cfg[2, ])
  expect_equal(df$ref_value_min,
               c(28602.36, 28602.36, 28602.36, 28602.36),
               tolerance = 0.1)
  expect_equal(df$check_value_min,
               c(4795, 3139, 3356, 4744),
               tolerance = 1)
  expect_equal(df$check,
               c("red", "yellow", "yellow", "red"))

  # relative model comparison ####
  df <- validateScenarios(dat_path, cfg[3, ])
  expect_equal(df$ref_value_min,
               c(31958, 33346),
               tolerance = 1)
  expect_equal(df$check_value_min,
               c(0.045035, -0.048130),
               tolerance = 0.0001)
  expect_equal(df$check,
               c("green", "green"))

  # multiple model comparisons ####
  selected_config <- cfg[3, ]
  selected_config$model <- "Model1,Model2"
  df <- validateScenarios(dat_path, selected_config)
  expect_equal(df$check, c("green", "green", "green", "green"))

  # relative scenario comparison ####
  df <- validateScenarios(dat_path, cfg[4, ])
  expect_equal(df$ref_value_min,
               c(31741, 33346),
               tolerance = 1)
  expect_equal(df$check_value_min,
               c(0.052188, -0.041615),
               tolerance = 0.0001)
  expect_equal(df$check,
               c("green", "green"))

  # multiple scenario comparisons ####
  selected_config <- cfg[4, ]
  selected_config$scenario <- "Scen1,Scen2"
  df <- validateScenarios(dat_path, selected_config)
  expect_equal(df$check, c("green", "green", "green", "green"))

  # relative period comparison ####
  df <- validateScenarios(dat_path, cfg[5, ])
  expect_equal(df$ref_value_min,
               c(37547.21, 36064.11, 36863.14, 37782.74),
               tolerance = 0.1)
  expect_equal(df$check_value_min,
               c(-0.11050, -0.11986, -0.13304, -0.11741),
               tolerance = 0.0001)
  expect_equal(df$check,
               c("cyan",  "cyan",  "blue", "cyan"))

  # difference model comparison ####
  df <- validateScenarios(dat_path, cfg[6, ])
  expect_equal(df$ref_value_min,
               c(31958, 33346),
               tolerance = 1)
  expect_equal(df$check_value_min,
               c(1439.3, -1605.0),
               tolerance = 1)
  expect_equal(df$check,
               c("green", "cyan"))

  # difference scenario comparison ####
  df <- validateScenarios(dat_path, cfg[7, ])
  expect_equal(df$ref_value_min,
               c(31741, 33346),
               tolerance = 1)
  expect_equal(df$check_value_min,
               c(1656.5, -1387.7),
               tolerance = 1)
  expect_equal(df$check,
               c("green", "green"))

  # difference period comparison ####
  df <- validateScenarios(dat_path, cfg[8, ])
  expect_equal(df$ref_value_min,
               c(37547.21, 36064.11, 36863.14, 37782.74),
               tolerance = 0.1)
  expect_equal(df$check_value_min,
               c(-4149.2, -4322.7, -4904.5, -4436.3),
               tolerance = 1)
  expect_equal(df$check,
               c("blue",  "blue",  "blue", "blue"))

  # absolute comparison ####
  df <- validateScenarios(dat_path, cfg[9, ])
  expect_equal(df$check_value_min,
               c(33398.0, 31741.4, 31958.7, 33346.4),
               tolerance = 1)
  expect_equal(df$check,
               c("red", "yellow", "yellow", "red"))

  # growthrate comparison ####
  # TODO: adjust after fixing issue 21
  df <- validateScenarios(dat_path, cfg[c(10,11), ])
  expect_equal(df$check_value_min,
               c(NA, NA, NA, NA, 0.01172299, 0.0288168, 0.0211624, 0.0160673),
               tolerance = 0.00001)
  expect_equal(df$check,
               c("grey", "grey", "grey", "grey", "cyan", "green", "green", "cyan"))


  # -> growthrate doesnt work for single time step atm, see
  # https://github.com/pik-piam/piamValidation/issues/21

})