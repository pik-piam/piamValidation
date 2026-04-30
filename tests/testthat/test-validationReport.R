test_that("validationReport accepts file path input", {

  data_file <- testthat::test_path("testdata", "REMIND_testdata.mif")

  expect_no_error(
    with_mocked_bindings(
      render = function(...) TRUE,
      {
        validationReport(
          dataPath = data_file,
          config = "default"
        )
      },
      .package = "rmarkdown"
    )
  )
})


test_that("validationReport accepts data.frame input", {

  dat <- importScenarioData(
    testthat::test_path("testdata", "REMIND_testdata.mif")
  )

  expect_no_error(
    with_mocked_bindings(
      render = function(...) TRUE,
      {
        validationReport(
          dataPath = dat,
          config = "default"
        )
      },
      .package = "rmarkdown"
    )
  )
})


test_that("validationReport accepts tibble config input", {

  dat <- importScenarioData(
    testthat::test_path("testdata", "REMIND_testdata.mif")
  )

  cfg <- getConfig("default")

  expect_no_error(
    with_mocked_bindings(
      render = function(...) TRUE,
      {
        validationReport(
          dataPath = dat,
          config = cfg
        )
      },
      .package = "rmarkdown"
    )
  )
})


test_that("validationReport passes params to render correctly", {

  dat <- importScenarioData(
    testthat::test_path("testdata", "REMIND_testdata.mif")
  )

  captured <- NULL

  with_mocked_bindings(
    render = function(input, params, output_file, ...) {
      captured <<- list(
        input = input,
        params = params,
        output_file = output_file
      )
      TRUE
    },
    {
      validationReport(
        dataPath = dat,
        config = "default"
      )
    },
    .package = "rmarkdown"
  )

  expect_true(is.list(captured))
  expect_true("mif" %in% names(captured$params))
  expect_true("cfg" %in% names(captured$params))
})


test_that("validationReport output filename contains object name", {

  mydata <- importScenarioData(
    testthat::test_path("testdata", "REMIND_testdata.mif")
  )

  outfile <- NULL

  with_mocked_bindings(
    render = function(input, params, output_file, ...) {
      outfile <<- basename(output_file)
      TRUE
    },
    {
      validationReport(
        dataPath = mydata,
        config = "default"
      )
    },
    .package = "rmarkdown"
  )

  expect_match(outfile, "mydata|R_object|validation")
})


test_that("validationReport supports multiple file inputs", {

  f1 <- testthat::test_path("testdata", "REMIND_testdata.mif")
  f2 <- testthat::test_path("testdata", "REMIND_testdata.mif")

  expect_no_error(
    with_mocked_bindings(
      render = function(...) TRUE,
      {
        validationReport(
          dataPath = c(f1, f2),
          config = "default"
        )
      },
      .package = "rmarkdown"
    )
  )
})
