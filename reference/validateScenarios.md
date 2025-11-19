# performs the validation checks from a config on a scenario data set

performs the validation checks from a config on a scenario data set

## Usage

``` r
validateScenarios(dataPath, config, outputFile = NULL, extraColors = TRUE)
```

## Arguments

- dataPath:

  one or multiple path(s) to scenario data in .mif or .csv format, in
  case of historic comparison, also path to reference data

- config:

  select config from inst/config or give a full path to a config file on
  your computer

- outputFile:

  give name of output file in case results should be exported; include
  file extension

- extraColors:

  if TRUE, use cyan and blue for violation of min thresholds instead of
  using the same colors as for max thresholds (yel and red)
