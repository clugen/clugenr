# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

library(ggplot2)   # For plotting in 2D
library(patchwork) # For combining 2D plots

# Possible alternative color scales if we want consistency between subplots with
# different number of clusters
color_scales <- list(seaborn =
                       c("1" = "#1F77B4", "2" = "#FF7F0E", "3" = "#2CA02C",
                         "4" = "#D62728", "5" = "#9467BD", "6" = "#8C564B",
                         "7" = "#E377C2", "8" = "#7F7F7F", "9" = "#BCBD22",
                         "10" = "#17BECF"),
                     set2 =
                       c("1" = "#66C2A5", "2" = "#FC8D62", "3" = "#8DA0CB",
                         "4" = "#E78AC3", "5" = "#A6D854", "6" = "#FFD92F",
                         "7" = "#E5C494", "8" = "#B3B3B3", "9" = "#A1C9F4",
                         "10" = "#4FB031"),
                     pastel =
                       c("1" = "#A1C9F4", "2" = "#FFB482", "3" = "#8DE5A1",
                         "4" = "#FF9F9B", "5" = "#D0BBFF", "6" = "#DEBB9B",
                         "7" = "#FAB0E4", "8" = "#CFCFCF", "9" = "#FFFEA3",
                         "10" = "#B9F2F0"),
                     husl9 =
                       c("1" = "#F67088", "2" = "#D58C31", "3" = "#A39F31",
                         "4" = "#4FB031", "5" = "#34AE90", "6" = "#36AAB5",
                         "7" = "#3BA3EC", "8" = "#BA82F4", "9" = "#F563D3",
                         "10" = "#FF7F0E"))

# Function for plotting a series of related 2D examples
plot_examples_2d <- function(...,
                             pmargin = 0.1,
                             clusters_field = "clusters",
                             palette = NULL) {

  # Place examples in a list
  ets <- list(...)

  # Was a specific palette specified?
  colscale <- if (is.null(palette)) {
    NULL
  } else {
    scale_fill_manual(values = color_scales[[palette]])
  }

  # Get maximum and minimum points in each dimension for all examples
  xmax <- max(sapply(ets, function(et) max(et$e$points[, 1])))
  xmin <- min(sapply(ets, function(et) min(et$e$points[, 1])))
  ymax <- max(sapply(ets, function(et) max(et$e$points[, 2])))
  ymin <- min(sapply(ets, function(et) min(et$e$points[, 2])))

  # Determine plots centers in each dimension
  xcenter <- (xmax + xmin) / 2
  ycenter <- (ymax + ymin) / 2

  # Determine plots span in both dimensions
  sidespan <- (1 + pmargin) * max(abs(xmax - xmin), abs(ymax - ymin)) / 2

  # Determine final plots limits in both dimensions
  xmax <- xcenter + sidespan
  xmin <- xcenter - sidespan
  ymax <- ycenter + sidespan
  ymin <- ycenter - sidespan

  # Create plots
  plts <- lapply(ets,
                 function(et) {
                   e <- et$e
                   t <- et$t
                   ggplot(NULL, aes(x = e$points[, 1], y = e$points[, 2])) +
                     geom_point(shape = 21, colour = "white", stroke = 0.1, alpha = 0.8,
                                aes(fill = e[[clusters_field]])) +
                     colscale +
                     xlab(NULL) + ylab(NULL) + ggtitle(t) +
                     theme(legend.position = "none",
                           plot.title = element_text(size = rel(0.7))) +
                     coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
                 })

  # Combine plots as subplots
  wrap_plots(plts)
}
