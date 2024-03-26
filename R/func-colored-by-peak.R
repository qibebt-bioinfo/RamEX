#' Color cells by special peak
#'
#' This function colors cells in a Ramanome object by a special peak in a specified reduction.
#'
#' @param object The Ramanome object
#' @param peak The special peak to color cells by
#' @param reduction The reduction method to use (default: 'UMAP')
#'
#' @return The plot object
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom paletteer scale_color_paletteer_c
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_bw
#' @export special_peak.coloring
special_peak.coloring <- function(object, peak, reduction = 'UMAP') {
  data.red <- object@reductions[[reduction]]
  names <- names(data.red)
  data.red$band <- object@interested.bands[[as.character(peak)]]
  cat(' Your interested peaks(bands) are: ', names(object@interested.bands), '\n',
      'Now the cells are colored by ', peak, 'cm-1 in ', reduction, 'reductions')
  plot <- data.red %>%
    ggplot(aes(get(names[1]), get(names[2]))) +
    geom_point(aes(color = band), size = 1) +
    paletteer::scale_color_paletteer_c("oompaBase::jetColors") +
    labs(x = '', y = '', color = paste(peak, ' cm-1')) +
    theme_bw() +
    SelfTheme
  ggsave(
    filename = paste0(reduction, 'colored by ', peak, ' cm-1', ".png"),
    plot = plot,
    width = 8,
    height = 6
  )
  return(plot)
}