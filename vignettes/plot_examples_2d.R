# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

library(ggplot2)   # For plotting in 2D
library(patchwork) # For combining 2D plots

# Function for plotting a series of related 2D examples
plot_examples_2d <- function(..., pmargin = 0.1) {
  ets <- list(...)
  xmax <- max(sapply(ets, function(et) max(et$e$points[, 1])))
  xmin <- min(sapply(ets, function(et) min(et$e$points[, 1])))
  ymax <- max(sapply(ets, function(et) max(et$e$points[, 2])))
  ymin <- min(sapply(ets, function(et) min(et$e$points[, 2])))
  xd <- pmargin * abs(xmax - xmin)
  yd <- pmargin * abs(ymax - ymin)
  xmax <- xmax + xd
  xmin <- xmin - xd
  ymax <- ymax + yd
  ymin <- ymin - yd
  plts <- lapply(
    ets,
    function(et) {
      e <- et$e
      t <- et$t
      ggplot(NULL, aes(x = e$points[, 1], y = e$points[, 2])) +
        geom_point(shape = 21, colour = "black", stroke = 0.1,
                   aes(fill = e$clusters)) +
        xlab(NULL) + ylab(NULL) + ggtitle(t) +
        theme(legend.position = "none",
              plot.title = element_text(size = rel(0.75))) +
        coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
    })
  wrap_plots(plts)
}
