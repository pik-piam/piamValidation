# import IAM data for validation

import IAM data for validation

## Usage

``` r
importScenarioData(scenarioPath, variables = NULL)
```

## Arguments

- scenarioPath:

  one or multiple paths to .mif, .csv, .rds or .xlsx file(s) or a
  data.frame containing scenario data in IAM format

- variables:

  optional character vector of variable names (may contain "\*"
  wildcards as in the config); if given, all other variables are dropped
  while reading so that only the data needed for the validation is held
  in memory
