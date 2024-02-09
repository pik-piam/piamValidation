# Validation Tools for PIK-PIAM

R package **piamValidation**, version **0.0.2**

[![CRAN status](https://www.r-pkg.org/badges/version/piamValidation)](https://cran.r-project.org/package=piamValidation)  [![R build status](https://github.com/pik-piam/piamValidation/workflows/check/badge.svg)](https://github.com/pik-piam/piamValidation/actions) [![codecov](https://codecov.io/gh/pik-piam/piamValidation/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/piamValidation) 

## Purpose and Functionality

The piamValidation package provides validation tools for the Potsdam Integrated Assessment Modelling environment.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("piamValidation")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Pascal Weigmann <pascal.weigmann@pik-potsdam.de>.

## Citation

To cite package **piamValidation** in publications use:

Weigmann P (2024). _piamValidation: Validation Tools for PIK-PIAM_. R package version 0.0.2, <URL: https://github.com/pik-piam/piamValidation>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {piamValidation: Validation Tools for PIK-PIAM},
  author = {Pascal Weigmann},
  year = {2024},
  note = {R package version 0.0.2},
  url = {https://github.com/pik-piam/piamValidation},
}
```
