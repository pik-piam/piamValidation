# Combine scenario and reference data with thresholds

for one row of cfg: filter and merge relevant scenario data with cfg
results in one df that contains scenario data, reference data and
thresholds

## Usage

``` r
combineData(scenData, cfgRow, histData = NULL)
```

## Arguments

- scenData:

  scenario data for one variable

- cfgRow:

  one row of a config file

- histData:

  reference data
