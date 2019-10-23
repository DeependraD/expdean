# ========== corstars ==========
#
#' @title Correlation table with stars
#' @description
#' Annotate table of correlation coefficients with the stars corresponding to the level of significance.
#' @param x A matrix (or all numeric dataframe) containing the data.
#' @param align A character vector of alignment values as is specified in `xtable(x, align)`.
#' @param caption A character vector to be used as caption for the correlation table.
#' @param method A character indicating method for obtaining correlation coefficient. Values can be either "pearson" or "spearman".
#' @param removeTriangle A character vector indicating which part of the table to remove. "upper" or "lower" triangle.
#' @param result A character indicating table output format. "html" or "latex" or "none" for plain markdown table.
#'
#' @return
#' @export
#'
#' @importFrom Hmisc rcorr
#' @importFrom xtable xtable
#'
#' @examples
#' \dontrun{
#' # require(tidyverse)
#' corstars(mtcars %>% select_if(is.numeric))
#' }
#'
corstars <-function(x, align, caption, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  x <- as.matrix(x)
  correlation_matrix<-Hmisc::rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value

  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))

  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]

  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")

  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }

  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }

  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable::xtable(Rnew), type="html", comment = FALSE)
    else print(xtable::xtable(Rnew, auto = TRUE,  align = align, caption = caption), type="latex",
               comment = FALSE, size = "\\footnotesize",
               sanitize.colnames.function = function(x){
                 paste0('{\\footnotesize{\\bfseries ', x, '}}')},
               booktabs = TRUE, tabular.environment="longtable",
               floating=FALSE)
  }
}
