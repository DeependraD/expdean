# ========== mean_differences_lm ==========
#
#' @title A combination of mean comparison/separation and visualization techniques
#'
#' @description This package tidies up regression output to give mean summary table, using various other packages in cohert
#' linear model and mean separation using DMRT/LSD or alike tests, from `agricolae` package
#' this function works best in conjuction with `lm_list`
#'
#' @param model_list A list of linear model (\code{\link{lm}}) objects.
#' This list must be named with respective response variable name in order that dataframe contains tidy names for columns
#' @param treatment_factor_list A list of single treatment factor or multiple treatment factors or interaction between two treatment factors.
#' In any way the terms fitted to the all response must all be same.
#'
#' @return A tbl
#' @export
#'
#' @importFrom purrr map_dfr pluck map reduce
#' @importFrom agricolae duncan.test
#' @importFrom magrittr set_colnames
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr left_join
#'
#' @examples
#' \dontrun{
#' # model marginal mean
#' mt_trial <- mtcars %>%
#'   rownames_to_column("vehicle_name") %>%
#'   as_tibble() %>%
#'   mutate_at(c("vehicle_name", "vs", "am"), as.factor)
#'
#' mt_lm1 <- lm(`mpg`~`am`*`vs` + `hp`, data = mt_trial)
#' mt_lm2 <- lm(`drat`~`am`*`vs` + `hp` + `disp`, data = mt_trial)
#' mt_lm3 <- lm(`qsec`~`am`*`vs` + `hp`, data = mt_trial)
#'
#' mt_named_list <- list(mt_lm1, mt_lm2, mt_lm3) %>%
#'   set_names(c("response_1st", "response_2nd", "response_3rd"))
#'
#' mt_treatment_factor_list <- list("am", "vs", c("am", "vs"))
#'
#' # main effects and interaction effects means for two factors using lm tidier
#' mean_differences_lm(model_list = mt_named_list, treatment_factor_list = mt_treatment_factor_list)
#' }
#'
mean_differences_lm <- function(model_list, treatment_factor_list){
  map_inner <- function(y){
    purrr::map_dfr(treatment_factor,
            function(x){
              agricolae::duncan.test(y, trt = x) %>%
                purrr::pluck("groups") %>%
                magrittr::rownames_to_column("treatment")
            })
  }

  model_means_df <- purrr::map(model_list, map_inner) %>%
    purrr::reduce(dplyr::left_join, by = "treatment") %>%
    # use name of model in list to clean variables after joining
    magrittr::set_colnames(c("treatment", paste(rep(names(model_list), each = 2), c("", "_group"), sep = "")))

  return(model_means_df)
}

# ========== mean_differences_emmeans ==========
#
#' @title Tidies up a list of lme (or lm) models to obtain table of mean comparison
#'
#' @description
#' This function relies extensively on `emmeans` package (yes, it works for lm too)
#'
#' @param model_list A list of model objects (\code{\link{lme}}, \code{\link{lmer}} or \code{\link{lm}})
#' @param treatment_factor_list A list of treatment factor(s) for which contrasts are specified in the model
#'
#' @return A tbl
#' @export
#'
#' @importFrom purrr map map_dfr
#' @importFrom tidyr as_tibble
#' @importFrom emmeans emmeans
#' @importFrom multcomp cld
#' @importFrom dplyr mutate_at vars contains
#' @examples
#' \dontrun{
#' # main effects and interaction effects means for two factors using emmeans tidier
#' mean_differences_emmeans(model_list = mt_named_list, treatment_factor_list = mt_treatment_factor_list)
#'
#' # plot emmeans
#' plot(emmeans::emmeans(mt_lm1, "am"), comparison = T)
#'
#' # plot emmeans p-values
#' emmeans::pwpp(emmeans::emmeans(mt_lm1, "am")) %>% plot()
#' }
#'
mean_differences_emmeans <- function(model_list, treatment_factor_list){
  # groups representing differences between means are valid only upto same treatment-effect type
  mean_differences_marginal <- purrr::map(model_list,
      function(model_item){
        purrr::map_dfr(treatment_factor_list,
                function(treatment)tidyr::as_tibble(multcomp::cld(emmeans::emmeans(model_item, treatment))), .id = "treatment_effect_type")
      }
  )

  mean_differences_marginal_df <- mean_differences_marginal %>%
    purrr::map_dfr(~.x %>%
        # select(c("treatment_description", "emmean", ".group")) %>%
        tidyr::as_tibble(),
        .id = "response") %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("group")), list(~as.character(as.numeric(.))))

  return(mean_differences_marginal_df)
}
