# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

library(ggplot2)   # For plotting in 2D
library(patchwork) # For combining 2D plots

# Function for plotting a series of related 2D examples
plot_examples_2d <- function(..., pmargin = 0.1) {

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
        xlab(NULL) + ylab(NULL) + ggtitle(t) +
        theme(legend.position = "none",
              plot.title = element_text(size = rel(0.7))) +
        coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
    })

  # Combine plots as subplots
  wrap_plots(plts)
}
