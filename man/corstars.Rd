% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corstars.R
\name{corstars}
\alias{corstars}
\title{Correlation table with stars}
\usage{
corstars(
  x,
  align,
  caption,
  method = c("pearson", "spearman"),
  removeTriangle = c("upper", "lower"),
  result = c("none", "html", "latex")
)
}
\arguments{
\item{x}{A matrix (or all numeric dataframe) containing the data.}

\item{align}{A character vector of alignment values as is specified in `xtable(x, align)`.}

\item{caption}{A character vector to be used as caption for the correlation table.}

\item{method}{A character indicating method for obtaining correlation coefficient. Values can be either "pearson" or "spearman".}

\item{removeTriangle}{A character vector indicating which part of the table to remove. "upper" or "lower" triangle.}

\item{result}{A character indicating table output format. "html" or "latex" or "none" for plain markdown table.}
}
\value{
A dataframe
}
\description{
Annotate table of correlation coefficients with the stars corresponding to the level of significance. A commonplace notation is use for annotation; For,
- p < .0001: "****"
- p < .001: "***"
- p < .01, "**", and
- p < .05, "*"
}
\examples{
\dontrun{
# require(tidyverse)
corstars(mtcars %>% select_if(is.numeric))
}

}
