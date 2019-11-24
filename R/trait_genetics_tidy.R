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
#' df <- expdean::simple_rcbd_df
#' df <- mutate_at(df, c("Genotype", "Rep"), as.factor)
#' # check for factor levels
#' df %>%
#'   select_if(list(~is.factor(.)|is.character(.))) %>%
#'   map_int(~length(unique(.x)))
#' # calculate genetic correlation for locations in absence of other grouping factors while adjusting for covariate
#' heritability_n_blues(df, grouping_items_sel = c("Buenos Aires", "Beijin", "Karaj"),
#'                      grouping_var = Location,
#'                      genotype = Genotype, replication_var = Rep,
#'                      dependent_var = YLD, covariate = TGW)
#'
#' @note Adapted from 'meta-r' application package
heritability_n_blues <- function(df, grouping_var, grouping_items_sel,
                                 dependent_var, covariate = NULL, genotype_var, replication_var){

  grvariable <- enquo(grouping_var) # capture the user expression and its environment
  response <- enquo(dependent_var)
  co_variate <- enquo(covariate)
  genotype <- enquo(genotype_var)
  replication <- enquo(replication_var)
  response_exp <- paste0(quo_name(response), "_exp")
  co_variate_scaled <- paste0(quo_name(co_variate), "_scaled")

  # create working dataframe with response as expectation of the interested variable
  df_tmp <- df %>%
    dplyr::mutate(!! response_exp := (!!response-mean(!!response, na.rm = T))/stats::sd(!!response, na.rm = T)) %>%
    dplyr::mutate(!! co_variate_scaled := scale(!!co_variate, scale = T, center = T))

  models <- df_tmp %>%
    dplyr::filter(!!grvariable %in% grouping_items_sel) %>%
    dplyr::group_by(!!grvariable) %>%
    dplyr::summarise(
      model_rcbd_random = list(
        lme4::lmer(stats::reformulate(termlabels = c(paste("(1|", !!(quo_name(replication)), ")"),
                                                     paste("(1|", !!(quo_name(genotype)), ")"),
                                                     !!(quo_name(co_variate_scaled))),
                                      response = !!(quo_name(response_exp))))),
      model_rcbd_mixed = list(
        lme4::lmer(stats::reformulate(termlabels = c(paste("(1|", !!(quo_name(replication)), ")"),
                                                     !!(quo_name(genotype)),
                                                     !!(quo_name(co_variate_scaled))),
                                      response = !!(quo_name(response_exp)))))
    )

  # instantiate objects and parameters
  genotypic_var <- numeric()
  error_var <- numeric()
  nRep <- df_tmp %>% dplyr::pull(!!replication) %>% unique() %>% length()

  # the fully random version of the model gives heritability estimates
  h2 <- purrr::map_dbl(.x = rlang::set_names(models$model_rcbd_random, pull(models, rlang::as_name(grvariable))),
                        .f = function(m){
                          genotypic_var <- purrr::as_vector(lme4::VarCorr(m)[rlang::as_name(genotype)]) # genotypic variance
                          error_var <- attr(lme4::VarCorr(m), "sc")^2 # error variance
                          heritability <- genotypic_var/(genotypic_var + error_var/nRep) # heritability
                          return(heritability)
                        })
  ## mixed version of the model is used for BLUEs estimation, which will later be used to calculate phenotypic correlations
  blue_mat <- purrr::map_dfc(models$model_rcbd_mixed, .f = function(x)lme4::fixef(x)) %>%
    magrittr::set_names(models %>% pull(!!grvariable))

  # generate correlation among replicates, if any are perfectly correlated
  # select all BLUE variables with more than 50% filled (or the those selected from grouping variable only)
  fixed_blue_mat <- blue_mat %>%
    as_tibble() %>%
    select_at(colnames(.)[map_dbl(.,
                                  ~(sum(is.na(.x))))/nrow(.) < 0.5])

  # also filter heritability estimates only for selected columns in blue matrix
  h2 <- h2[colnames(fixed_blue_mat)]

  corPHEN <- cor(fixed_blue_mat, use="pairwise.complete.obs")
  glPHEN <- nrow(fixed_blue_mat)
  if(glPHEN<=2) glPHEN <- 3
  tPHEN <- abs(corPHEN/sqrt((1-corPHEN^2)/(glPHEN-2)))
  pvaluePHEN <- 2*(1-pt(tPHEN,glPHEN-2))

  covPHEN <- cov(fixed_blue_mat, use="pairwise.complete.obs")

  ## calculate genetic correlation for locations if grouping factor is present
  # Genetic correlations
  corGEN <- matrix(NA,ncol=length(grouping_items_sel),nrow=length(grouping_items_sel))
  dimnames(corGEN) <- list(colnames(corPHEN),colnames(corPHEN))
  gl <- length(grouping_items_sel)
  if(gl<=2) gl <- 3

  # For each phenotypic correlation, adjust it to generate genotypic correlation
  for(j in 1:length(grouping_items_sel)){
    for(k in j:length(grouping_items_sel)){
      corGEN[j,k] <- corGEN[k,j] <- covPHEN[j,k]/sqrt(h2[j]*h2[k])
    }
  }

  # is this way to calculate pvalues for genotypic correlation coefficients correct ?
  diag(corGEN) <- 1
  corGEN[corGEN > 1] <- .9999
  corGEN[corGEN < -1] <- -0.9999
  tGEN <- abs(corGEN/sqrt((1-corGEN^2)/(gl-2)))
  pvalueGEN <- 2*(1-pt(tGEN,gl-2))

  # The phenotypic correlation matrix is
  corPHEN

  # The genotypic correlation matrix is
  corGEN

  out <- list(heritability = h2, blues = blue_mat,
              corphen = corPHEN, corphen_pvalue = pvaluePHEN,
              corgen = corGEN, corgen_pvalue = pvalueGEN)
  return(out)
}
