# Warn if reference data required by the config is missing from input data

Compares the reference models of all historical checks in the config
against the models found in the reference data provided by the user
(scenario "historical") and warns about missing ones. If reference data
files shipped with the package contain the missing models, they are
suggested.

## Usage

``` r
checkRefData(cfg, hist)
```

## Arguments

- cfg:

  processed config as used in “validateScenarios()“

- hist:

  historical/reference data as used in “validateScenarios()“
