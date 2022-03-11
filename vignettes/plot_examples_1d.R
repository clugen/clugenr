# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

library(ggplot2)   # For plotting in 2D
library(patchwork) # For combining 2D plots

# Function for plotting the 1D clusters, showing density
plot_examples_1d <- function(..., pmargin = 0.1, ymax = 0.6) {

  # Place examples in a list
  ets <- list(...)

  # Get maximum and minimum points in each dimension for all examples
  xmax <- max(sapply(ets, function(et) max(et$e$points)))
  xmin <- min(sapply(ets, function(et) min(et$e$points)))

  # Determine plot margins beyond maximum and minimum points
  xd <- pmargin * abs(xmax - xmin)
  xmax <- xmax + xd
  xmin <- xmin - xd

  # Create plots
  plts <- lapply(
    ets,
    function(et) {
      e <- et$e
      t <- et$t
      ggplot() +
        geom_density(mapping = aes(x = e$points,
                                   colour = e$clusters,
                                   fill = e$clusters),
                     alpha = 0.3) +
        geom_point(mapping = aes(x = e$points,
                                 y = -0.02,
                                 fill = e$clusters),
                   shape = 21,
                   colour = "black",
                   stroke = 0.1,
                   alpha = 0.2) +
        ggtitle(t) + xlab(NULL) + ylab(NULL) +
        xlim(xmin, xmax) + ylim(-0.025, ymax) +
        theme(legend.position = "none",
              plot.title = element_text(size = rel(0.75)),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank()
        )
    })

  # Combine plots as subplots
  wrap_plots(plts)
}
