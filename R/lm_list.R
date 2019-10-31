# ========== lm_list ==========
#
#' @title Fit linear models for multiple response variables in a dataframe
#' Function creates from a vector of response variables in a given dataframe, a list of linear model objects.
#' Additionally, a character vector of RHS terms of linear model formula specification, and a dataset needs to be passed.
#'
#' @param df A dataframe or any other object of coercible class
#' @param response A character vector of response variables to fit a common formula to using `lm`.
#' @param rhs_terms A character vector. Used in specifying the model in the same order that was passed.
#' Each element of the vector will be passed as a model term in the \code{\link{formula}} separated by "+".
#'
#' @return List of \code{\link{lm}} objects
#' @export
#'
#' @importFrom janitor clean_names
#' @importFrom stringr str_trim
#' @importFrom stringi stri_replace_all_fixed
#' @importFrom magrittr set_names
#' @importFrom purrr map
#' @importFrom stats reformulate
#'
#' @examples
#' \dontrun{
#' mt_trial <- mtcars %>%
#'   rownames_to_column("vehicle_name") %>%
#'   as_tibble() %>%
#'   mutate_at(c("vehicle_name", "vs", "am"), as.factor)
#'
#' lm_list(mt_trial, c("mpg", "drat", "qsec"), c("am", "vs", "am:vs", "hp"))
#'
#' }
#'
lm_list <- function(df, response, rhs_terms){

  response_orig <- response # orignal frame response names
  rhs_terms_orig <- rhs_terms # original frame right hand side terms
  rhs_terms_split <- stringr::str_trim(unlist(rhs_terms_orig %>% str_split("[*:|/+]"))) # original frame formula terms
  df <- df[, c(rhs_terms_split, response)]
  cols_orig <- colnames(df) # original frame colnames
  response_icol <- which(cols_orig %in% response_orig) # column index of response in original frame
  rhs_terms_i <- which(cols_orig %in% rhs_terms_split) # original frame formula terms index
  df <- janitor::clean_names(df)
  response_clean <- colnames(df)[response_icol]
  rhs_terms_split_clean <- colnames(df)[rhs_terms_i]
  rhs_terms_clean <- stringi::stri_replace_all_fixed(rhs_terms_orig,
                                                     pattern =  rhs_terms_split,
                                                     replacement = rhs_terms_split_clean,
                                                     vectorize_all=FALSE)
  # note that formula argument does not accept special characters as it is
  # it need to be supplied as formula syntax using `formula()`
  # the function accepts literal strings and as argument/s to convert them to syntax
  # however, when using `reformulate()` any special character needs to be `quote()`ed.
  # and if special characters such as parenthesis are `quote()`ed, and passed
  # in no way quoted expressions are treated as literal strings (or term name only)
  # for example, log transformation could be applied in either side of
  # the formula using `quote()`
  # so better use simple literal strings as term names
  model_list <- purrr::map(.x = response_clean,
                    .f = function(x){
                      stats::lm(stats::reformulate(response = x,
                                     termlabels = rhs_terms_clean),
                         data = df)
                    }
  )
  return(magrittr::set_names(model_list, response_clean))
}
