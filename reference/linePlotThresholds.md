# takes the output of "validateScenarios()" creates a line plot

takes the output of "validateScenarios()" creates a line plot

## Usage

``` r
linePlotThresholds(
  valiData,
  scenData = NULL,
  refData = NULL,
  xlim = c(2010, 2030),
  interactive = TRUE
)
```

## Arguments

- valiData:

  data to be plotted, as returned by “validateScenarios()“ and after
  filtering for one variable and one region.

- scenData:

  hand over additional scenario data to be plotted alongside the
  validation data. Will use the same variable and region, otherwise all
  available data.

- refData:

  hand over additional reference data to be plotted alongside the
  validation data. Will use the same variable and region, otherwise all
  available data.

- xlim:

  set limits for the x axis

- interactive:

  decide if an interactive ggploty object should be returned
