# takes the output of "validateScenarios()" and computes threshold bands

converts the thresholds attached to each data point by
“validateScenarios()“ into absolute values per variable, unit, region
and period. The resulting bands can be plotted as colored background
areas behind scenario data, see “linePlotThresholds()“ or the
“thresholds“ argument of “mip::createLinePlots()“.

## Usage

``` r
getThresholdBands(valiData, warnMultiple = TRUE)
```

## Arguments

- valiData:

  data.frame as returned by “validateScenarios()“

- warnMultiple:

  if TRUE, warn in case multiple sets of thresholds are found for the
  same variable, unit, region and period, e.g. from checks using
  multiple metrics or reference scenarios; plotting such bands can lead
  to overlaps

## Value

data.frame with columns variable, unit, region, period, min_red,
min_yel, max_yel and max_red

## Details

For checks using the metrics "relative" and "difference", thresholds are
converted into absolute values using the reference values of the
respective check. Thresholds which are not defined in the config appear
as -Inf/Inf.
