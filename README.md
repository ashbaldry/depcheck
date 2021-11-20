# `depcheck`

<!-- badges: start -->
[![R-CMD-check](https://github.com/ashbaldry/depcheck/workflows/R-CMD-check/badge.svg)](https://github.com/ashbaldry/depcheck/actions)
[![Codecov test coverage](https://codecov.io/gh/ashbaldry/depcheck/branch/main/graph/badge.svg)](https://codecov.io/gh/ashbaldry/depcheck?branch=main)
<!-- badges: end -->

The aim of `{depcheck}` is to check package dependencies, and flag any packages that are rarely utilised or never called.

## Installation

```r
devtools::install_github("ashbaldry/depcheck")
```

## Usage

### Package Check

```r
library(depcheck)
checkPackageDependencies()
```

### Shiny Application Check

```r
library(depcheck)
checkShinyDependencies()
```
