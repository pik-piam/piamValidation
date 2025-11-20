# Validation Tools for PIK-PIAM

R package **piamValidation**, version **0.15.4**

[![CRAN
status](https://www.r-pkg.org/badges/version/piamValidation)](https://cran.r-project.org/package=piamValidation)
[![R build
status](https://github.com/pik-piam/piamValidation/workflows/check/badge.svg)](https://github.com/pik-piam/piamValidation/actions)
[![codecov](https://codecov.io/gh/pik-piam/piamValidation/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/piamValidation)
[![r-universe](https://pik-piam.r-universe.dev/badges/piamValidation)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

The piamValidation package provides validation tools for the Potsdam
Integrated Assessment Modelling environment.

## Installation

For installation of the most recent package version an additional
repository has to be added in R:

``` r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```

The additional repository can be made available permanently by adding
the line above to a file called `.Rprofile` stored in the home folder of
your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using
`install.packages`:

``` r
install.packages("piamValidation")
```

Package updates can be installed using `update.packages` (make sure that
the additional repository has been added before running that command):

``` r
update.packages()
```

## Tutorial

The package comes with vignettes describing the basic functionality of
the package and how to use it. You can load them with the following
command (the package needs to be installed):

``` r
vignette("publication")       # Publication
vignette("validateScenarios") # validateScenarios
```

## Questions / Problems

In case of questions / problems please contact Pascal Weigmann
<pascal.weigmann@pik-potsdam.de>.

## Citation

To cite package **piamValidation** in publications use:

Weigmann P, Richters O, Lécuyer F, Koch J (2025). “piamValidation:
Validation Tools for PIK-PIAM.” Version: 0.15.4,
<https://pik-piam.github.io/piamValidation%20https://github.com/pik-piam/piamValidation>.

A BibTeX entry for LaTeX users is

``` latex
@Misc{,
 title = {piamValidation: Validation Tools for PIK-PIAM},
 author = {Pascal Weigmann and Oliver Richters and Fabrice Lécuyer and Johannes Koch},
 date = {2025-11-20},
 year = {2025},
 url = {https://pik-piam.github.io/piamValidation https://github.com/pik-piam/piamValidation},
 note = {Version: 0.15.4},
}
```
