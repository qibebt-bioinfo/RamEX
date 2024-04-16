#' Perform data reduction use pca method on Ramanome object
#'
#' This function performs data reduction on a Ramanome object using PCA
#'
#' @param object A Ramanome object.
#' @param draw Logical value to indicate whether to draw the reduction plot. Default is FALSE.
#'
#' @return The updated Ramanome object with the reduced data added to the reductions slot.
#'
#' @export
#' @import ggplot2
#' @import stats
reductions.pca <- function(object, draw = FALSE) {
  dataset <- get.nearest.dataset(object)
  data.red <- data.frame(stats::prcomp(dataset, scale = TRUE, retx = TRUE)$x[, 1:2])


  name <- names(object@reductions)
  object@reductions <- append(object@reductions, list(data.red))
  names(object@reductions) <- c(name, method)

  if (draw) {
    names <- colnames(data.red)
    plot <- ggplot(data.red, aes(get(names[1]), get(names[2]), color = as.factor(object@meta.data$group))) +
      geom_point() +
      theme_bw() +
      stat_ellipse(level = 0.8) +
      labs(x = names[1], y = names[2]) +
      theme(
        panel.grid = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.text = element_markdown(size = 15, family = "myFont"),
        legend.background = element_blank(),
        text = element_text(color = "black"),
        axis.text.x = element_text(size = 15, angle = 0, family = "myFont"),
        axis.text.y = element_text(size = 15, family = "myFont"),
        axis.ticks = element_line(linewidth = 1),
        axis.ticks.length = unit(0.4, "lines"),
        axis.title = element_text(family = "myFont", size = 15)
      )
    ggsave(paste("Reduction.pca.png"), plot, width = 8, height = 6)
  }

  return(object)
}

#' Perform data reduction use tSNE method on Ramanome object
#'
#' This function performs data reduction on a Ramanome object using tSNE
#'
#' @param object A Ramanome object.
#' @param draw Logical value to indicate whether to draw the reduction plot. Default is FALSE.
#'
#' @return The updated Ramanome object with the reduced data added to the reductions slot.
#'
#' @export
#' @import ggplot2
#' @import Rtsne
reductions.tsne <- function(object, draw = FALSE) {
  dataset <- get.nearest.dataset(object)

  data.red <- data.frame(Rtsne::Rtsne(
    dataset,
    dims = 2,
    perplexity = 5,
    theta = 0.5,
    verbose = FALSE,
    max_iter = 10000
  )$Y)
  colnames(data.red) <- c("tSNE 1", "tSNE 2")
  name <- names(object@reductions)
  object@reductions <- append(object@reductions, list(data.red))
  names(object@reductions) <- c(name, method)

if (draw) {
  names <- colnames(data.red)
  plot <- ggplot(data.red, aes(get(names[1]), get(names[2]), color = as.factor(object@meta.data$group))) +
    geom_point() +
    theme_bw() +
    stat_ellipse(level = 0.8) +
    labs(x = names[1], y = names[2]) +
    theme(
      panel.grid = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_blank(),
      legend.text = element_markdown(size = 15, family = "myFont"),
      legend.background = element_blank(),
      text = element_text(color = "black"),
      axis.text.x = element_text(size = 15, angle = 0, family = "myFont"),
      axis.text.y = element_text(size = 15, family = "myFont"),
      axis.ticks = element_line(linewidth = 1),
      axis.ticks.length = unit(0.4, "lines"),
      axis.title = element_text(family = "myFont", size = 15)
    )
  ggsave(paste("Reduction.tsne.png"), plot, width = 8, height = 6)
  }

return(object)
}

#' Perform data reduction use UMAP method on Ramanome object
#'
#' This function performs data reduction on a Ramanome object using UMAP
#'
#' @param object A Ramanome object.
#' @param draw Logical value to indicate whether to draw the reduction plot. Default is FALSE.
#'
#' @return The updated Ramanome object with the reduced data added to the reductions slot.
#'
#' @export
#' @import ggplot2
#' @import uwot
reductions.umap <- function(object, draw = FALSE) {
  dataset <- get.nearest.dataset(object)
  data.red <- data.frame(uwot::umap(dataset))
  colnames(data.red) <- c("UMAP 1", "UMAP 2")


  name <- names(object@reductions)
  object@reductions <- append(object@reductions, list(data.red))
  names(object@reductions) <- c(name, method)

if (draw) {
  names <- colnames(data.red)
  plot <- ggplot(data.red, aes(get(names[1]), get(names[2]), color = as.factor(object@meta.data$group))) +
    geom_point() +
    theme_bw() +
    stat_ellipse(level = 0.8) +
    labs(x = names[1], y = names[2]) +
    theme(
      panel.grid = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_blank(),
      legend.text = element_markdown(size = 15, family = "myFont"),
      legend.background = element_blank(),
      text = element_text(color = "black"),
      axis.text.x = element_text(size = 15, angle = 0, family = "myFont"),
      axis.text.y = element_text(size = 15, family = "myFont"),
      axis.ticks = element_line(linewidth = 1),
      axis.ticks.length = unit(0.4, "lines"),
      axis.title = element_text(family = "myFont", size = 15)
    )
  ggsave(paste("Reduction.umap.png"), plot, width = 8, height = 6)
}

return(object)
}
