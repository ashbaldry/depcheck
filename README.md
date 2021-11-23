# `depcheck`

<!-- badges: start -->
[![R-CMD-check](https://github.com/ashbaldry/depcheck/workflows/R-CMD-check/badge.svg)](https://github.com/ashbaldry/depcheck/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test coverage](https://codecov.io/gh/ashbaldry/depcheck/branch/main/graph/badge.svg)](https://codecov.io/gh/ashbaldry/depcheck?branch=main)
<!-- badges: end -->

The goal of `{depcheck}` is to provide an overview of the R packages used within a project, finds which functions have been called from the loaded packages, and includes a warning for any package that is seldom/never utilised.

## Installation

```r
devtools::install_github("ashbaldry/depcheck")
```

## Usage

The 3 main functions in `{depcheck}`:

- `checkPackageDependencyUse()` will read the Depends and Imports fields from the `DESCRIPTION` file, and look in the `R` subdirectory for package usage.
- `checkShinyDependencyUse()` will check all relevant R files for `library`, `require`, `requireNamespace` and `::` calls to find all packages used, and then run a check on total usage of those packages. 
- `checkProjectDependencyUse()` is a more generic version of `checkShinyDependencyUse()` and will recursively search all R files in a project directory for packages and their use.

When printing the results, there are a few options to adjust what is classified as "low usage":

- `warn_percent_usage`, the minimum percentage of functions used before the warning flag is produced
- `warn_number_usage`, the minimum number of functions used before the warning flag is produced
- `ignore_low_usage_packages`, if already aware of a package that is rarely used, then it can be included here so it won't be printed as low usage

If a package fails all 3 conditions, then it will be included in the list of under utilised packages.

**NB** Base R packages, such as `{utils}` and `{stats}` will automatically be excluded from the low usage packages 

## Example

```r
project_dependencies <- checkShinyDependencyUse("../reddit-analysis-app") # ashbaldry/reddit-analysis-app
summary(project_dependencies)
# Number of Declared Packages: 14
# Total Number of Dependencies: 85
# Declared Packages: utils, glue, httr, highcharter, scales, shiny.semantic, htmlwidgets, stringi, 
# quanteda, R6, data.table, shiny, promises, magrittr
# Function usage for 'glue', 'htmlwidgets', 'stringi', 'magrittr' are below the specified thresholds. 
# Print individual package summaries to check if packages can be removed

summary(project_dependencies$glue)
# Package: 'glue'
# Package Dependencies: 0
# Package Usage: 1 / 16 (6%)
# Functions Used: glue
# Function usage for 'glue' is below the specified thresholds. Consider copying used function to reduce dependencies
```
