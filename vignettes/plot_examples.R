# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

library(ggplot2)   # For plotting in 2D
library(patchwork) # For combining 2D plots
library(rgl)       # For plotting in 3D

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
                   aes(fill = e$point_clusters)) +
        xlab(NULL) + ylab(NULL) + ggtitle(t) +
        theme(legend.position = "none",
              plot.title = element_text(size = rel(0.75))) +
        coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
    })
  wrap_plots(plts)
}

# Function for plotting a series of related 3D examples
plot_examples_3d <- function(..., pmargin = 0.1, ncols = 3) {

  ets <- list(...)

  xmax <- max(sapply(ets, function(et) max(et$e$points[, 1])))
  xmin <- min(sapply(ets, function(et) min(et$e$points[, 1])))
  ymax <- max(sapply(ets, function(et) max(et$e$points[, 2])))
  ymin <- min(sapply(ets, function(et) min(et$e$points[, 2])))
  zmax <- max(sapply(ets, function(et) max(et$e$points[, 3])))
  zmin <- min(sapply(ets, function(et) min(et$e$points[, 3])))
  xd <- pmargin * abs(xmax - xmin)
  yd <- pmargin * abs(ymax - ymin)
  zd <- pmargin * abs(zmax - zmin)
  xmax <- xmax + xd
  xmin <- xmin - xd
  ymax <- ymax + yd
  ymin <- ymin - yd
  zmax <- zmax + zd
  zmin <- zmin - zd

  if (length(ets) %% ncols != 0) {
    stop("Number of examples must be a multiple of `ncols`")
  }

  nrows <- length(ets) %/% ncols

  mfrow3d(nrows, ncols, sharedMouse = T)

  plts <- lapply(
    ets,
    function(et) {
      e <- et$e
      t <- et$t
      plot3d(e$points, type = "s", size = 1.5,
             col = e$point_clusters, aspect = T,
             xlab = "x", ylab = "y", zlab = "z", main = t,
             xlim = c(xmin, xmax), ylim = c(ymin, ymax), zlim = c(zmin, zmax))
    }
  )

  highlevel(integer()) # To trigger display as rglwidget
}

# Function for plotting the 1D clusters, showing density
plot_examples_1d <- function(..., pmargin = 0.1, ymax = 0.6) {
  ets <- list(...)
  xmax <- max(sapply(ets, function(et) max(et$e$points)))
  xmin <- min(sapply(ets, function(et) min(et$e$points)))
  xd <- pmargin * abs(xmax - xmin)
  xmax <- xmax + xd
  xmin <- xmin - xd
  plts <- lapply(
    ets,
    function(et) {
      e <- et$e
      t <- et$t
      ggplot() +
        geom_density(mapping = aes(x = e$points,
                                   colour = e$point_clusters,
                                   fill = e$point_clusters),
                     alpha = 0.3) +
        geom_point(mapping = aes(x = e$points,
                                 y = -0.02,
                                 fill = e$point_clusters),
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
  wrap_plots(plts)
}

# Function for plotting the nD clusters
plot_examples_nd <- function(e, title, pstroke = 0.05, psize = 0.75) {

  # How many dimensions?
  nd <- ncol(e$points)

  # All possible combinations
  idxs <- expand.grid(1:nd, 1:nd)

  plts <- apply(idxs,
                1,
                function(x) {
                  if (x[1] == x[2]) {
                    grid::textGrob(paste0("x", x[1]))
                  }
                  else {
                    ggplot(NULL,
                           aes(x = e$points[, x[1]],
                               y = e$points[, x[2]],
                               colour = e$point_clusters)) +
                      geom_point(shape = 21,
                                 colour = "black",
                                 stroke = pstroke,
                                 size = psize,
                                 aes(fill = e$point_clusters)) +
                      xlab(NULL) + ylab(NULL) +
                      theme(legend.position = "none",
                            axis.ticks.x = element_blank(),
                            axis.text.x = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.text.y = element_blank()) +
                      coord_fixed()
                  }
                })
  wrap_plots(plts) + plot_annotation(
    title = title,
    theme = theme(plot.title = element_text(size = rel(0.8))))
}
