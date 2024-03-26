#' Draw mean spectrum
#'
#' This function generates a plot of the mean spectrum for a Ramanome object.
#'
#' @param object The Ramanome object
#' @param gap The gap between the mean spectra (default: 0)
#'
#' @import ggplot2
#' @importFrom plyr arrange
#' @importFrom plyr join
#' @importFrom plyr
#' @importFrom stats aggregate
#'
#' @return The plot of the mean spectrum
#'
#' @export
setGeneric("draw.mean", function(object,gap=NA) standardGeneric("draw.mean"))

setMethod("draw.mean", signature(object = "Ramanome"), function(object, gap=0) {
  plot <- mean.spec(get.nearest.dataset(object), object@meta.data$group, gap)
  ggsave(
    'mean spec.png',
    plot,
    width = 10,
    height = 8
  )
})

#' Generate mean spectrum
#'
#' This function calculates the mean spectrum for a given Ramanome object.
#'
#' @param data The data matrix
#' @param group The group factor
#' @param gap The gap between the mean spectra (default: 0.3)
#'
#' @import hyperSpec
#' @import ggplot2
#' @import ggtext
#' @return The mean spectrum
#' @export
mean.spec <- function(data, group, gap = 0.3) {
  levels <- levels(group)
  group <- as.character(group)
  print(levels)
  data <- as.matrix(data)
  spec_mean <- aggregate(data, by = list(group), mean)
  spec_sd <- aggregate(data, by = list(group), sd)
  n <- nrow(spec_mean)
  i <- which(spec_mean[, 1] == levels[1])
  data_ggplot <- cbind(as.numeric(colnames(data)), t(spec_mean[i, -1] + gap * (n - 1)), t(spec_sd[i, -1]), spec_mean$Group.1[i])
  j <- 2
  for (level in levels[-1]) {
    i <- which(spec_mean[, 1] == level)
    data_ggplot <- rbind(
      data_ggplot,
      cbind(
        as.numeric(colnames(data)),
        t(spec_mean[i, -1] + gap * (n - j)),
        t(spec_sd[i, -1]),
        spec_mean$Group.1[i]
      )
    )
    j <- j + 1
  }
  
  data_ggplot <- as.data.frame(data_ggplot)
  colnames(data_ggplot) <- c('wavenumber', 'value', 'sd', 'Group')
  data_ggplot$wavenumber <- as.numeric(data_ggplot$wavenumber)
  data_ggplot$value <- as.numeric(data_ggplot$value)
  data_ggplot$sd <- as.numeric(data_ggplot$sd)
  data_ggplot$Group <- factor(data_ggplot$Group, levels = levels)
  plot <- ggplot(
    data = data_ggplot,
    aes(
      x = wavenumber,
      y = value,
      group = Group
    )
  ) +
    geom_ribbon(
      aes(
        ymin = value - sd,
        ymax = value + sd,
        fill = Group
      ),
      alpha = 0.3
    ) +
    geom_line(
      aes(color = Group),
      linewidth = 0.8
    ) +
    theme_bw() +
    labs(y = "Normalized Intensity (a.u.)") +
    xlab(expression(paste("Wavenumber (cm"^{ -1 }, ")"))) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = c(500,1000,1500,2000, 2500,3000,3500)
    ) +
    theme(
      panel.grid = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_blank(),
      legend.text = element_markdown(size = 15),
      legend.background = element_blank(),
      text = element_text(color = "black"),
      axis.title.y = element_text(size = 20, angle = 90),
      axis.text.x = element_text(size = 15, angle = 0),
      axis.text.y = element_blank(),
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(size = 20),
      axis.ticks = element_line(linewidth = 1),
      axis.ticks.y = element_blank(),
      axis.ticks.length = unit(0.4, "lines"),
      axis.title = element_text(size = 20)
    )
  
  return(plot)
}
