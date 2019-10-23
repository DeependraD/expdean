# ========== long_stargazer ==========
#
#' @title Long stargazer
#' @description Easy and useful wrapper around stargazer that instantly embeds given
#' latex table in a longtable environment.
#' @param ... Arguments to be passed regularly in `stargazer::stargazer`
#'
#' @return TeX code
#' @export
#'
#' @importFrom stringr str_replace
#' @importFrom stargazer stargazer
#'
#' @examples
#' @note Must use this function with float=FALSE in stargazer
#' @note While using stargazer in a chunk use results = "asis" in option.
#'
#' \dontrun{
#' # some nice defaults for longstargazer
# long_stargazer(some_model_or_list_of_models,
#                title = "Some title",
#                style = "all", digits = 2,
#                header = FALSE, font.size = "small",
#                column.labels = c("\\parbox[t]{2.0cm}{Response 1}", # parbox will keep names within fixed width
#                                  "\\parbox[t]{2.0cm}{Response 2}"),
#                model.names = TRUE,
#                dep.var.labels.include = FALSE,
#                digits.extra = 3,
#                covariate.labels = c("Factor 1 level 1", "Factor 1 level 2", "Factor 2 level 1", "Factor 2 level 2"),
#                align = TRUE,
#                single.row = TRUE, # compacts all summary stats in a single row
#                float.env = "table*",
#                df = FALSE,
#                omit.stat = c("adj.rsq"), # alternatively "keep.stat" may be used to show few stats only
#                column.sep.width = "1pt",
#                float = TRUE,
#                no.space = TRUE) # "no.space" to remove empty lines
#' }
#'
long_stargazer <- function(...){
  output <- capture.output(stargazer::stargazer(...))
  # The lines 4 and second last lines are the ones we want to remove...
  output[c(4, length(output)-1)] <- output[c(4, length(output)-1)] %>%
    stringr::str_replace("tabular", "longtable")
  # cat out the results - this is essentially just what stargazer does too
  cat(paste(output, collapse = "\n"), "\n")
}
