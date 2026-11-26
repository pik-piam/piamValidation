test_that(
  "scenario data in CSV format is correctly imported",
  {
    # target data
    data <- quitte::as.quitte(
      dplyr::tribble(
        ~model, ~scenario, ~region, ~variable, ~unit, ~period, ~value,
        "REMIND", "Test" , "World",   "FE",   "EJ/yr",  2005,    10,
        "REMIND", "Test" , "World",   "FE",   "EJ/yr",  2010,    11
      )
    )
    # test data
    data_csv_comma <- importScenarioData(
      testthat::test_path("testdata", "REMIND_testdata_comma.csv"))
    expect_equal(data, data_csv_comma)

    data_csv_semicolon <- importScenarioData(
      testthat::test_path("testdata", "REMIND_testdata_semicolon.csv"))
    expect_equal(data, data_csv_semicolon)
})

test_that(
  "scenario data in MIF format is correctly imported",
  {
    # target data
    data <- quitte::as.quitte(
      dplyr::tribble(
        ~model, ~scenario, ~region, ~variable, ~unit, ~period, ~value,
        "REMIND", "Test" , "World",   "FE",   "EJ/yr",  2005,    10,
        "REMIND", "Test" , "World",   "FE",   "EJ/yr",  2010,    11
      )
    )
    # test data
    data_mif <- importScenarioData(
      testthat::test_path("testdata", "REMIND_testdata.mif"))
    expect_equal(data, data_mif)
  })

test_that(
  "scenario data in XLSX format is correctly imported",
  {
    # target data
    data <- quitte::as.quitte(
      dplyr::tribble(
        ~model, ~scenario, ~region, ~variable, ~unit, ~period, ~value,
        "REMIND", "Test" , "World",   "FE",   "EJ/yr",  2005,    10,
        "REMIND", "Test" , "World",   "FE",   "EJ/yr",  2010,    11
      )
    )
    # test data
    data_xlsx <- importScenarioData(
      testthat::test_path("testdata", "REMIND_testdata.xlsx")
    )
    expect_equal(data, data_xlsx)
  })

test_that(
  "scenario data can be loaded from RDS files",
  {
    # target data
    data <- quitte::as.quitte(
      dplyr::tribble(
        ~model, ~scenario, ~region, ~variable, ~unit, ~period, ~value,
        "REMIND", "Test" , "World",   "FE",   "EJ/yr",  2005,    10,
        "REMIND", "Test" , "World",   "FE",   "EJ/yr",  2010,    11
      )
    )
    # test data
    data_rds <- importScenarioData(
      testthat::test_path("testdata", "REMIND_testdata.rds")
    )
    expect_equal(data, data_rds)
  })

test_that(
  "scenario data can be R data.frame",
  {
    # target data
    data <- quitte::as.quitte(
      dplyr::tribble(
        ~model, ~scenario, ~region, ~variable, ~unit, ~period, ~value,
        "REMIND", "Test" , "World",   "FE",   "EJ/yr",  2005,    10,
        "REMIND", "Test" , "World",   "FE",   "EJ/yr",  2010,    11
        )
      )
    # test data
    data_df <- importScenarioData(
      dplyr::tibble(model = c("REMIND", "REMIND"),
                     scenario = c("Test", "Test"),
                     region = c("World", "World"),
                     variable = c("FE", "FE"),
                     unit = c("EJ/yr", "EJ/yr"),
                     period = c(2005, 2010),
                     value = c(10, 11)
                    )
      )
    expect_equal(data, data_df)
  })
