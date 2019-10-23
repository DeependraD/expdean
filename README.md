
# expdean

<!-- badges: start -->
<!-- badges: end -->

The *ex*periment *de*sign and *an*alysis package is meant for dealing with data analysis of plant science (Agriculture and Genetics) and allied field. This is born out of personal use necessicity, mainly.

## Installation

You can install the released version of expdean from [Github](https://github.com/DeependraD/expdean) with:

``` r
devtools::install_github("DeependraD/expdean")
```

## Using `lm_list()` function

``` r
library(expdean)

lm_list()
```

# Agricultural research experiments analysis workflow

1. `lm_list`: Fit linear models for multiple response variables in a dataframe
2. `corstars`: Correlation coefficient table with significance annotation
2. `grid_arrange_share_legend`: Share legend and plot multiple ggplot grobs
4. `mean_differences_lm`, `mean_differences_emmeans`: Mean separation techniques for multiple. Some visualization techniques are also included as comment.
5. `p_annotator`: Model summary table for various model types]. This implements tidy techniques for regression model summary table generation (Looks alike stargazer summary table.
6. `long_stargazer`: Model summary table with modified stargazer and some useful defaults.
7. [Example script for multiple column pasting in designed way](scripts/paste_together_multicolumns_by_index.R)


# Trait genetics with R

To work with a variety of experimental design and response traits observed from those experiments, this repository hosts a fair amount of analytical and tidifying solutions.

# Checklist

- In tidy version, resolve covariate specification -- function shall run without any covariate or with multiple covariates.
