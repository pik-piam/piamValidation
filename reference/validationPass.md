# returns information on whether scenarios passed critical validation checks

returns information on whether scenarios passed critical validation
checks

## Usage

``` r
validationPass(data, yellowFail = FALSE)
```

## Arguments

- data:

  data.frame as returned from “validateScenarios()“

- yellowFail:

  if set to TRUE a yellow check result of a critical variable will lead
  to the scenario not passing as validated
