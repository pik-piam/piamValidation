# perform validateScenarios and create an .html report using .Rmd templates

perform validateScenarios and create an .html report using .Rmd
templates

## Usage

``` r
validationReport(
  dataPath,
  config,
  report = "default",
  outputDir = "output",
  extraColors = TRUE
)
```

## Arguments

- dataPath:

  one or multiple path(s) to scenario data in .mif or .csv format

- config:

  name a config from inst/config ("validationConfig\_\<name\>.csv") or
  give a full path to a separate configuration file

- report:

  name a .Rmd from inst/markdown ("validationReport\_\<name\>.Rmd") to
  be rendered or give a full path to a separate .Rmd file

- outputDir:

  choose a directory to save validation reports to

- extraColors:

  if TRUE, use cyan and blue for violation of min thresholds instead of
  using the same colors as for max thresholds (yel and red)
