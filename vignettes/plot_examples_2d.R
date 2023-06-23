# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

library(ggplot2)   # For plotting in 2D
library(patchwork) # For combining 2D plots

# Possible alternative color scales if we want consistency between subplots with
# different number of clusters
color_scales = list(
  seaborn = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
              "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF"),
  set2 = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854",
           "#FFD92F", "#E5C494", "#B3B3B3", "#A1C9F4", "#4FB031"),
  pastel = c("#A1C9F4", "#FFB482", "#8DE5A1", "#FF9F9B", "#D0BBFF",
             "#DEBB9B", "#FAB0E4", "#CFCFCF", "#FFFEA3", "#B9F2F0"),
  husl9 = c("#F67088", "#D58C31", "#A39F31", "#4FB031", "#34AE90",
            "#36AAB5", "#3BA3EC", "#BA82F4", "#F563D3", "#FF7F0E"))

# Function for plotting a series of related 2D examples
plot_examples_2d <- function(..., pmargin = 0.1, palette = NULL) {

  # Was a specific palette specified?
  colscale <- if (is.null(palette)) {
    NULL
  } else {
    scale_fill_manual(values = color_scales[[palette]])
  }

  # Place examples in a list
  ets <- list(...)

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
  plts <- lapply(
    ets,
    function(et) {
      e <- et$e
      t <- et$t
      ggplot(NULL, aes(x = e$points[, 1], y = e$points[, 2])) +
        geom_point(shape = 21, colour = "white", stroke = 0.1, alpha = 0.8,
                   aes(fill = e$clusters)) +
        colscale +
        xlab(NULL) + ylab(NULL) + ggtitle(t) +
        theme(legend.position = "none",
              plot.title = element_text(size = rel(0.7))) +
        coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
    })

  # Combine plots as subplots
  wrap_plots(plts)
}
