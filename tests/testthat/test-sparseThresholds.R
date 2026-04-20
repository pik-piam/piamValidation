test_that("sparse thresholds are applied correctly", {

  # starting with a one-line config with 4 thresholds, test whether the result
  # stays the same when removing 1, 2 or 3 of these thresholds.
  # test this for all possible color outcomes
  cfg <- testthat::test_path("testdata", "validationConfig_testSparseThresholds.csv")
  data <- testthat::test_path("testdata", "REMIND_testdata.rds")  # value = 10

  # configs that should always return a certain color
  # validateScenario has extraColors = TRUE by default, including cyan and blue
  cfg_green  <- suppressMessages(getConfig(cfg))
  cfg_yellow <- mutate(cfg_green, max_yel = 5)
  cfg_red    <- mutate(cfg_green, max_yel = 5, max_red = 6)
  cfg_cyan   <- mutate(cfg_green, min_yel = 15)
  cfg_blue   <- mutate(cfg_green, min_yel = 15, min_red = 14)

  configs <- data.frame(
    rbind(cfg_green, cfg_yellow, cfg_red, cfg_cyan, cfg_blue),
    row.names = c("green", "yellow", "red", "cyan", "blue")
    )

  # do tests for all colors
  for (color in row.names(configs)) {
    cfg_color <- tibble::as_tibble(configs[color, ])

    # iterate through all possibilities of threshold usage:
    # min 1, max 4 thresholds per row.
    # NA gets replaced by "+/-INF" for calculations
    cfg_1110 <- cfg_color %>% mutate(max_red = NA)
    cfg_1101 <- cfg_color %>% mutate(max_yel = NA)
    cfg_1011 <- cfg_color %>% mutate(min_yel = NA)
    cfg_0111 <- cfg_color %>% mutate(min_red = NA)

    cfg_1100 <- cfg_color %>% mutate(max_red = NA, max_yel = NA)
    cfg_1001 <- cfg_color %>% mutate(max_yel = NA, min_yel = NA)
    cfg_0011 <- cfg_color %>% mutate(min_yel = NA, min_red = NA)
    cfg_0110 <- cfg_color %>% mutate(min_red = NA, max_red = NA)

    cfg_1000 <- cfg_color %>% mutate(max_red = NA, max_yel = NA, min_yel = NA)
    cfg_0001 <- cfg_color %>% mutate(max_yel = NA, min_yel = NA, min_red = NA)
    cfg_0010 <- cfg_color %>% mutate(min_yel = NA, min_red = NA, max_red = NA)
    cfg_0100 <- cfg_color %>% mutate(min_red = NA, max_red = NA, max_yel = NA)

    # results should only change if the threshold of the expected color is
    # removed, e.g. if max_red is removed (=Inf), a "red" will become "yellow"
    if (color == "green") {
      expect_equal(validateScenarios(data, cfg_1110)$check, color)
      expect_equal(validateScenarios(data, cfg_1101)$check, color)
      expect_equal(validateScenarios(data, cfg_1011)$check, color)
      expect_equal(validateScenarios(data, cfg_0111)$check, color)

      expect_equal(validateScenarios(data, cfg_1100)$check, color)
      expect_equal(validateScenarios(data, cfg_1001)$check, color)
      expect_equal(validateScenarios(data, cfg_0011)$check, color)
      expect_equal(validateScenarios(data, cfg_0110)$check, color)

      expect_equal(validateScenarios(data, cfg_1000)$check, color)
      expect_equal(validateScenarios(data, cfg_0001)$check, color)
      expect_equal(validateScenarios(data, cfg_0010)$check, color)
      expect_equal(validateScenarios(data, cfg_0100)$check, color)

    } else if (color == "red") {
      expect_equal(validateScenarios(data, cfg_1110)$check, "yellow")
      expect_equal(validateScenarios(data, cfg_1101)$check, color)
      expect_equal(validateScenarios(data, cfg_1011)$check, color)
      expect_equal(validateScenarios(data, cfg_0111)$check, color)

      expect_equal(validateScenarios(data, cfg_1100)$check, "green")
      expect_equal(validateScenarios(data, cfg_1001)$check, color)
      expect_equal(validateScenarios(data, cfg_0011)$check, color)
      expect_equal(validateScenarios(data, cfg_0110)$check, "yellow")

      expect_equal(validateScenarios(data, cfg_1000)$check, "green")
      expect_equal(validateScenarios(data, cfg_0001)$check, color)
      expect_equal(validateScenarios(data, cfg_0010)$check, "yellow")
      expect_equal(validateScenarios(data, cfg_0100)$check, "green")

    } else if (color == "yellow") {
      expect_equal(validateScenarios(data, cfg_1110)$check, color)
      expect_equal(validateScenarios(data, cfg_1101)$check, "green")
      expect_equal(validateScenarios(data, cfg_1011)$check, color)
      expect_equal(validateScenarios(data, cfg_0111)$check, color)

      expect_equal(validateScenarios(data, cfg_1100)$check, "green")
      expect_equal(validateScenarios(data, cfg_1001)$check, "green")
      expect_equal(validateScenarios(data, cfg_0011)$check, color)
      expect_equal(validateScenarios(data, cfg_0110)$check, color)

      expect_equal(validateScenarios(data, cfg_1000)$check, "green")
      expect_equal(validateScenarios(data, cfg_0001)$check, "green")
      expect_equal(validateScenarios(data, cfg_0010)$check, color)
      expect_equal(validateScenarios(data, cfg_0100)$check, "green")

    } else if (color == "cyan") {
      expect_equal(validateScenarios(data, cfg_1110)$check, color)
      expect_equal(validateScenarios(data, cfg_1101)$check, color)
      expect_equal(validateScenarios(data, cfg_1011)$check, "green")
      expect_equal(validateScenarios(data, cfg_0111)$check, color)

      expect_equal(validateScenarios(data, cfg_1100)$check, color)
      expect_equal(validateScenarios(data, cfg_1001)$check, "green")
      expect_equal(validateScenarios(data, cfg_0011)$check, "green")
      expect_equal(validateScenarios(data, cfg_0110)$check, color)

      expect_equal(validateScenarios(data, cfg_1000)$check, "green")
      expect_equal(validateScenarios(data, cfg_0001)$check, "green")
      expect_equal(validateScenarios(data, cfg_0010)$check, "green")
      expect_equal(validateScenarios(data, cfg_0100)$check, color)

    } else if (color == "blue") {
      expect_equal(validateScenarios(data, cfg_1110)$check, color)
      expect_equal(validateScenarios(data, cfg_1101)$check, color)
      expect_equal(validateScenarios(data, cfg_1011)$check, color)
      expect_equal(validateScenarios(data, cfg_0111)$check, "cyan")

      expect_equal(validateScenarios(data, cfg_1100)$check, color)
      expect_equal(validateScenarios(data, cfg_1001)$check, color)
      expect_equal(validateScenarios(data, cfg_0011)$check, "green")
      expect_equal(validateScenarios(data, cfg_0110)$check, "cyan")

      expect_equal(validateScenarios(data, cfg_1000)$check, color)
      expect_equal(validateScenarios(data, cfg_0001)$check, "green")
      expect_equal(validateScenarios(data, cfg_0010)$check, "green")
      expect_equal(validateScenarios(data, cfg_0100)$check, "cyan")
    }
  }
})
