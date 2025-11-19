# takes the output of "validateScenarios()" and plots heat maps per variable

takes the output of "validateScenarios()" and plots heat maps per
variable

## Usage

``` r
validationHeatmap(
  valiData,
  main_dim = "variable",
  x_plot = NULL,
  y_plot = NULL,
  x_facet = NULL,
  y_facet = NULL,
  interactive = TRUE
)
```

## Arguments

- valiData:

  data to be plotted, as returned by “validateScenarios()“ (and
  “appendTooltips()“ if interactive), plus optional filtering. Needs to
  have at least one dimension with only one unique element.

- main_dim:

  out of the 5-dim df, 1 dim has to contain only on element, this is the
  main dimension of the plot, default: variable

- x_plot:

  choose dimension to display on x-axis of plot, if any is NULL,
  arrangement is chosen automatically based on data dimensions

- y_plot:

  choose dimension to display on y-axis of plot

- x_facet:

  choose dimension to display on x-dim of facets

- y_facet:

  choose dimension to display on x-dim of facets

- interactive:

  return plots as interactive plotly plots by default
