# evaluate the content of the "ref\_\<type\>" column and filter reference data accordingly. cases: - mode chosen - range - mean - no mode chosen - use mean if multiple references

returns df without variable, unit and \<type\> columns (see below)
returns df with ref_value_min/max, ref_model, ref_scenario, ref_period

## Usage

``` r
refineRefData(ref_data, cfgRow, ref_type = "ref_model")
```

## Arguments

- ref_data:

  pre-filtered reference data

- cfgRow:

  row of validation config used for this data slice

- ref_type:

  historical, model, scenario, period
