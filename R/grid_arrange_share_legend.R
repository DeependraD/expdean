# ========== grid_arrange_share_legend ==========
#
#' @title Common legend for two or more `gg` grobs.
#' @description Almost all parts of the function is borrowed from `vignette(package = "gridExtra", "arrangeGrob")`.
#' Refer to this vignettee for extra details.
#' @param ... Objects of class grob
#' @param nrow Number of rows to align figures in.
#' @param ncol Number of columns to align figures in.
#' @param position Position where the legend will be printed. "bottom" or
#' "right"
#'
#' @return A grob
#' @export
#'
#' @importFrom gridExtra arrangeGrob
#' @importFrom grid unit.c grid.draw grid.newpage
#' @importFrom ggplot2 ggplotGrob theme
#'
#' @examples
#' \dontrun{
#' # Example
#' mtmodified <- mtcars %>%
#' mutate_at(c("am", "vs"), as.factor)
#' mtmodified1 <- mtmodified %>% ggplot(aes(am, mpg, group = `am`, fill = `am`)) + geom_boxplot()
#' mtmodified2 <- mtmodified %>% ggplot(aes(vs, mpg, group = `vs`, fill = `vs`)) + geom_boxplot()
#' grid_arrange_share_legend(mtmodified1, mtmodified2,
#'                           nrow = 2, ncol = 1, position = "bottom")
#' }
#' @note Source: http://rpubs.com/sjackman/grid_arrange_shared_legend
#'
grid_arrange_share_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplot2::ggplotGrob(plots[[1]] + ggplot2::theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)

  combined <- switch(position,
                     "bottom" = gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = grid::unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = grid::unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid::grid.newpage()
  grid::grid.draw(combined)
}
