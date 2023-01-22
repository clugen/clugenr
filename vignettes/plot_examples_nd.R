# Copyright (c) 2020-2023 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

library(ggplot2)   # For plotting in 2D
library(patchwork) # For combining 2D plots

# Function for plotting the nD clusters
plot_examples_nd <- function(e, title, pstroke = 0.05, psize = 0.75,
                             pmargin = 0.1) {

  # How many dimensions?
  nd <- ncol(e$points)

  # Get maximum and minimum points in each dimension
  xmaxs <- apply(e$points, 2, max)
  xmins <- apply(e$points, 2, min)

  # Determine plots centers in each dimension
  xcenters <- (xmaxs + xmins) / 2

  # Determine plot span in all dimensions
  sidespan <- (1 + pmargin) * max(abs(xmaxs - xmins)) / 2

  # Determine final plots limits in all dimensions
  xmaxs <- xcenters + sidespan
  xmins <- xcenters - sidespan

  # All possible combinations
  idxs <- expand.grid(1:nd, 1:nd)

  # Create plots
  plts <- apply(idxs,
                1,
                function(x) {
                  if (x[1] == x[2]) {
                    grid::textGrob(paste0("x", x[1]))
                  } else {
                    ggplot(NULL,
                           aes(x = e$points[, x[2]],
                               y = e$points[, x[1]],
                               colour = e$clusters)) +
                      geom_point(shape = 21,
                                 colour = "black",
                                 stroke = pstroke,
                                 size = psize,
                                 aes(fill = e$clusters)) +
                      xlab(NULL) + ylab(NULL) +
                      theme(legend.position = "none",
                            axis.ticks.x = element_blank(),
                            axis.text.x = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.text.y = element_blank()) +
                      coord_fixed(xlim = c(xmins[x[2]], xmaxs[x[2]]),
                                  ylim = c(xmins[x[1]], xmaxs[x[1]]))
                  }
                })

  # Combine plots as subplots
  wrap_plots(plts) + plot_annotation(
    title = title,
    theme = theme(plot.title = element_text(size = rel(0.8))))
}
