# ========== pca_biplot ==========
#
#' @title It takes a prcomp object and returns a PCA biplot for selected variables
#' @description This function is a convenience function for quick plotting of PCA biplot
#'
#' @param A `prcomp` object returned by prcomp()
#' @param x Name of first principal component vector. i.e. x = "PC1"
#' @param y Name of second principal component vector. i.e. y = "PC2"
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#'
#' \dontrun{
#' PCbiplot(stats::prcomp(dfp %>%
#'                   group_by(genotype_id) %>%
#'                   summarise_if(is.numeric, mean, na.rm = T) %>%
#'                   select_at(which(map_lgl(., is.numeric))) %>%
#'                   select(-contains("rep")) %>%
#'                   scale())) +
#'   theme_bw() +
#'   theme(text = element_text(size = 15))
#' }
#'

# pca biplot
PCbiplot <- function(PC, x="PC1", y="PC2") {
  # PC being a prcomp object
  data <- data.frame(PC$x)
  plot <- ggplot2::ggplot(data, aes(x=get(x), y=get(y))) +
    ggplot2::geom_point() +
    xlab("PC1") +
    ylab("PC2")
  plot <- plot + ggplot2::geom_hline(yintercept = 0, size=.2) + ggplot2::geom_vline(xintercept = 0, size=.2)
  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  plot <- plot + ggplot2::coord_equal() +
    ggrepel::geom_text_repel(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color="red")
  plot <- plot +
    ggplot2::geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2),
                 arrow=ggplot2::arrow(length=ggplot2::unit(0.2,"cm")), alpha=0.75, color="red")
  plot
}
