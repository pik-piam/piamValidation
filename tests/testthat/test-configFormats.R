test_that(
  "config in CSV format is correctly imported",
  {
    # target config
    config <- tibble::tribble(
        ~metric, ~critical, ~variable, ~unit, ~model, ~scenario, ~region, ~period,
        ~min_red, ~min_yel, ~max_yel, ~max_red, ~ref_model, ~ref_scenario, ~ref_period,
        "relative", "yes", "Emi|CO2|Energy", "Mt CO2/yr", NA, NA, NA, "2010",
        -0.2, -0.1,  0.15,   0.3, "CEDS", "historical", NA,

        "absolute", "yes", "Emi|CO2|Energy", "Mt CO2/yr", NA, NA, NA, "2010-2020",
        2000, 5000, 20000, 40000, NA, NA, NA
        )
    # test config
    config_csv_semicolon <- suppressMessages(getConfig(
      testthat::test_path("testdata", "validationConfig_testCSVsemicolon.csv")))
    expect_equal(config_csv_semicolon, config)
})

test_that(
  "config in XLSX format is correctly imported",
  {
    # target config
    config <- tibble::tribble(
      ~metric, ~critical, ~variable, ~unit, ~model, ~scenario, ~region, ~period,
      ~min_red, ~min_yel, ~max_yel, ~max_red, ~ref_model, ~ref_scenario, ~ref_period,
      "relative", "yes", "Emi|CO2|Energy", "Mt CO2/yr", NA, NA, NA, "2010",
      -0.2, -0.1,  0.15,   0.3, "CEDS", "historical", NA,

      "absolute", "yes", "Emi|CO2|Energy", "Mt CO2/yr", NA, NA, NA, "2010-2020",
      2000, 5000, 20000, 40000, NA, NA, NA
    )
    # test config
    config_xlsx <- suppressMessages(getConfig(
      testthat::test_path("testdata", "validationConfig_testExcel.xlsx")))
    expect_equal(config_xlsx, config)
  })

test_that(
  "config can be R data.frame",
  {
    # target config
    config <- tibble::tribble(
      ~metric, ~critical, ~variable, ~unit, ~model, ~scenario, ~region, ~period,
      ~min_red, ~min_yel, ~max_yel, ~max_red, ~ref_model, ~ref_scenario, ~ref_period,
      "relative", "yes", "Emi|CO2|Energy", "Mt CO2/yr", NA, NA, NA, "2010",
      -0.2, -0.1,  0.15,   0.3, "CEDS", "historical", NA,

      "absolute", "yes", "Emi|CO2|Energy", "Mt CO2/yr", NA, NA, NA, "2010-2020",
      2000, 5000, 20000, 40000, NA, NA, NA
    )
    # test config
    config_df <- suppressMessages(getConfig(config))
    expect_equal(config_df, config)
  })
