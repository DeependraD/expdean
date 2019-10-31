
# expdean

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The *ex*periment *de*sign and *an*alysis package is meant for dealing with data analysis of plant science (Agriculture and Genetics) and allied field. This is born out of personal use necessicity, mainly.

## Installation

You can install development version of the expdean package from [Github](https://github.com/DeependraD/expdean) with:

``` r
devtools::install_github("DeependraD/expdean")
library(expdean)
```

## Agricultural research experiments analysis workflow

1. `lm_list`: Fit linear models for multiple response variables in a dataframe
2. `corstars`: Correlation coefficient table with significance annotation; Carried over with slight modification from: http://myowelt.blogspot.com/2008/04/beautiful-correlation-tables-in-r.html
2. `grid_arrange_share_legend`: Share legend and plot multiple ggplot grobs; Copied from: http://rpubs.com/sjackman/grid_arrange_shared_legend
4. `mean_differences_lm`, `mean_differences_emmeans`: Mean separation techniques for multiple. Some visualization techniques are also included as comment.
5. `p_annotator`: Model summary table for various model types. This implements tidy techniques for regression model summary table generation (Looks alike stargazer summary table).
6. `long_stargazer`: Model summary table with modified stargazer and some useful defaults.
7. Mixed modeling when analyzing common experimental designs require estimation of heritability, BLUEs, phenotypic correlation and genotypic correlation. A solution for tidy calculation and representation of such results is implemented in `trait_genetics_tidy.R`.

## Checklist

1. Resolve covariate specification in `trait_genetics_tidy.R`.
- [] `heritability_n_blues()` function should accept `NULL` as well as arbitrary number of covariates.
- [] confirm the validity of `heritability_n_blues()` function in estimating heritability.
2. Currently only 3 models are tidied (in join step) due to column name mismatch in `model_summary_kabletype.R`'s `p_annotator()` function.
- [] Use metaprogramming to solve it.

## Building my first package

- When hosting a package remotely as a source repo we can confirm that the name we choose for the package is infact available by:

```
library(available)
available("expdean")
```
- `usethis::use_roxygen_md()` to use
- `usethis::use_data(some_object_usually_df)` to use object in current session as exported data.
- `usethis::use_tidy_eval()` to use rlang and NSE features without explicitly exporting each function
- `devtools::document()` to automatically generate roxygen documents and namespace file
- `usethis::use_mit_license()`
