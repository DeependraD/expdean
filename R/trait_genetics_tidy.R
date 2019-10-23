#' Compute heritability and genotypic BLUEs for each selected grouping variable adjusting for covariate.
#'
#' @param df A dataframe or similar object of coercible class.
#' @param grouping_var Column name of the grouping variable such as location or environment of character or factor type.
#' @param grouping_items_sel A character vector of items which are subset of grouping variable selected for obtaining heritability.
#' @param dependent_var Column name of response variable whose heritability estimates are being obtained through model.
#' @param covariate Column name of numeric variable. Adjusted values of dependent variable for this covariate will be used in obtaining estimates.
#' @param genotype Column name of variable representing genotypes/treatments.
#' @param replication_var Column name of variable representing replicated records of genotypes/treatments.
#'
#' @return A list
#' @export
#'
#' @importFrom rlang `!!` `:=`
#' @importFrom tidyr `%>%`
#' @importFrom dplyr mutate summarise pull filter group_by
#' @importFrom stats sd reformulate
#' @importFrom lme4 lmer
#' @importFrom purrr map2_dbl map_dfc
#' @importFrom magrittr set_names
#'
#' @examples
#'
#' library(lme4)
#' library(emmeans)
#' require(tidyverse)
#' # load data
#' df <- data(sample_rcbd) # na = ".", while reading the csv
#' df <- df %>%
#'   mutate_at(c("Genotype", "Rep"), as.factor)
#' # check for factor levels
#' df %>%
#'   select_if(funs(is.factor(.)|is.character(.))) %>%
#'   map_int(~length(unique(.x)))
#' # calculate genetic correlation for locations in absence of other grouping factors while adjusting for covariate
#' heritability_n_blues(df, grouping_items_sel = c("Buenos Aires", "Beijin", "Karaj"),
#'                      grouping_var = Location,
#'                      genotype = Genotype, replication_var = Rep,
#'                      dependent_var = YLD, covariate = TGW)
#'
#' @note Adapted from 'meta-r' application package
heritability_n_blues <- function(df, grouping_var, grouping_items_sel,
                                 dependent_var, covariate = FALSE, genotype_var, replication_var){

  grvariable <- dplyr::enquo(grouping_var)
  gritems <- dplyr::enquo(grouping_items_sel)
  response <- dplyr::enquo(dependent_var)
  co_variate <- dplyr::enquo(covariate)
  genotype <- enexpr(genotype_var)
  replication <- enexpr(replication_var)
  response_exp <- paste0(quo_name(response), "_exp")
  co_variate_scaled <- paste0(quo_name(co_variate), "_scaled")

  # create working dataframe with response as expectation of the interested variable
  df_tmp <- df %>%
    dplyr::mutate(!! response_exp := (!!response-mean(!!response, na.rm = T))/stats::sd(!!response, na.rm = T)) %>%
    dplyr::mutate(!! co_variate_scaled := scale(!!co_variate, scale = T, center = T))

  # instantiate objects and parameters
  genotypic_var <- numeric()
  error_var <- numeric()
  h2 <- numeric()
  nRep <- df_tmp %>% dplyr::pull(!!replication) %>% unique() %>% length()

  # loop to obtain heritability from random model and BLUEs from mixed model
  models <- df_tmp %>%
    dplyr::filter(!!grvariable %in% grouping_items_sel) %>%
    dplyr::group_by(!!grvariable) %>%
    dplyr::summarise(model_rcbd_random = list(
      lme4::lmer(stats::reformulate(termlabels = c(paste("(1|", replication_var, ")"),
                                      paste("(1|", genotype_var, ")"),
                                      co_variate_scaled),
                       response = response_exp))),
    model_rcbd_mixed = list(
      lmer(stats::reformulate(termlabels = c(paste("(1|", replication_var, ")"),
                                      genotype_var,
                                      co_variate_scaled), # to omit fixed effect covariate remove thismanually
                       response = response_exp)))
    )
  h2 <- purrr::map2_dbl(.x = models$model_rcbd_random,
                 .y = grouping_items_sel,
                 .f = function(m, n){
                   genotypic_var <- as.vector(VarCorr(m)$genotype_var) # genotypic variance
                   error_var <- attr(VarCorr(m), "sc")^2 # error variance
                   h2[n] <- genotypic_var/(genotypic_var + error_var/nRep) # heritability
                   })
  ## BLUEs for Phenotypic correlations
  blue_mat <- purrr::map_dfc(models$model_rcbd_mixed, .f = function(x)fixef(x)) %>%
    magrittr::set_names(models %>% pull(!!grvariable))

  out <- list(heritability = h2, blues = blue_mat)
  return(out)
}

# generate correlation among replicates, if any are perfectly correlated
# select all BLUE variables with more than 50% filled (or the those selected from grouping variable only)
fixed_blue_mat <- h2_blue$BLUES %>%
  as_data_frame() %>%
  select_at(colnames(.)[map_dbl(.,
                                ~(sum(is.na(.x))))/nrow(h2_blue$BLUES) < 0.5
                        ])
# save heritability values
h2 <- h2_blue$Heritability

corPHEN <- cor(fixed_blue_mat, use="pairwise.complete.obs")
glPHEN <- nrow(fixed_blue_mat)
if(glPHEN<=2) glPHEN <- 3
tPHEN <- abs(corPHEN/sqrt((1-corPHEN^2)/(glPHEN-2)))
pvaluePHEN <- 2*(1-pt(tPHEN,glPHEN-2))

## calculate genetic correlation for locations if grouping factor is present
# Genetic correlations
corGEN <- matrix(NA,ncol=length(selected),nrow=length(selected))
dimnames(corGEN) <- list(colnames(corPHEN),colnames(corPHEN))
gl <- length(selected)
if(gl<=2) gl <- 3

# For each phenotypic correlation, adjust it to generate genotypic correlation
for(j in 1:length(selected)){
  for(k in j:length(selected)){
    corGEN[j,k] <- corGEN[k,j] <- corPHEN[j,k]/sqrt(h2[j]*h2[k])
  }
}

diag(corGEN) <- 1
corGEN[corGEN > 1] <- .9999
corGEN[corGEN < -1] <- -0.9999
tGEN <- abs(corGEN/sqrt((1-corGEN^2)/(gl-2)))
pvalueGEN <- 2*(1-pt(tGEN,gl-2))

# The phenotypic correlation matrix is
corPHEN

# The genotypic correlation matrix is
corGEN
