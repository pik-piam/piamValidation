# Publication

A methodological paper with the title “Validation of Climate Mitigation
Pathways” is available as pre-print
[here](https://egusphere.copernicus.org/preprints/2025/egusphere-2025-2284/).

### Abstract

> Integrated assessment models (IAMs) are crucial for climate
> policymaking, offering climate mitigation scenarios and contributing
> to IPCC assessments. However, IAMs face criticism for lack of
> transparency and poor capture of recent technology diffusion and
> dynamics. We introduce the Potsdam Integrated Assessment Modeling
> validation tool, piamValidation, an open-source R package for
> validating IAM scenarios. The piamValidation tool enables systematic
> comparisons of variables from extensive IAM datasets against
> historical data and feasibility bounds, or across scenarios and
> models. This functionality is particularly valuable for harmonizing
> scenarios across multiple IAMs. Moreover, the tool facilitates the
> systematic comparison of near-term technology dynamics with external
> observational data, including historical trends, near-term
> developments, and stylized facts. We apply the tool to the integrated
> assessment model REMIND for near-term technology trend validation,
> demonstrating its potential to enhance transparency and reliability of
> IAMs.

## Reproduce Plots

The code and data to reproduce the plots in the publication are
available in the package. After installation, please follow these steps:

``` r
library(piamValidation)

# NGFS validation
scen_NGFS <- piamutils::getSystemFile("extdata/NGFS_scenario_data_publication.rds", 
                                        package = "piamValidation")
hist_NGFS <- piamutils::getSystemFile("extdata/NGFS_reference_data_publication.rds", 
                                        package = "piamValidation")
validationReport(dataPath = c(scen_NGFS, hist_NGFS), 
                 config = "publication_NGFS",
                 report = "publication_NGFS")

# REMIND near-term trends
scen_REMIND <- piamutils::getSystemFile("extdata/REMIND_scenario_data_publication.rds", 
                                        package = "piamValidation")
hist_REMIND <- piamutils::getSystemFile("extdata/REMIND_reference_data_publication.rds", 
                                        package = "piamValidation")
validationReport(dataPath = c(scen_REMIND, hist_REMIND), 
                 config = "publication",
                 report = "publication")
```

This creates two HTML reports with the respective plots in an `output`
folder in the current working directory.
