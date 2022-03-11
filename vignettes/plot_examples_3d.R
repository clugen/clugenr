# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

library(rgl)       # For plotting in 3D

# Function for plotting a series of related 3D examples
plot_examples_3d <- function(..., pmargin = 0.1, ncols = 3) {

  # Place examples in a list
  ets <- list(...)

  # Get maximum and minimum points in each dimension for all examples
  xmax <- max(sapply(ets, function(et) max(et$e$points[, 1])))
  xmin <- min(sapply(ets, function(et) min(et$e$points[, 1])))
  ymax <- max(sapply(ets, function(et) max(et$e$points[, 2])))
  ymin <- min(sapply(ets, function(et) min(et$e$points[, 2])))
  zmax <- max(sapply(ets, function(et) max(et$e$points[, 3])))
  zmin <- min(sapply(ets, function(et) min(et$e$points[, 3])))

  # Determine plots centers in each dimension
  xcenter <- (xmax + xmin) / 2
  ycenter <- (ymax + ymin) / 2
  zcenter <- (zmax + zmin) / 2

  # Determine plot span in all dimensions
  sidespan <- (1 + pmargin) * max(abs(xmax - xmin),
                                  abs(ymax - ymin),
                                  abs(zmax - zmin)) / 2

  # Determine final plots limits in all dimensions
  xmax <- xcenter + sidespan
  xmin <- xcenter - sidespan
  ymax <- ycenter + sidespan
  ymin <- ycenter - sidespan
  zmax <- zcenter + sidespan
  zmin <- zcenter - sidespan

  if (length(ets) %% ncols != 0) {
    stop("Number of examples must be a multiple of `ncols`")
  }

  # Determine number of rows for subplots in final plot
  nrows <- length(ets) %/% ncols

  # Combine plots as subplots with shared mouse and perspective
  mfrow3d(nrows, ncols, sharedMouse = T)

  # Create plots
  plts <- lapply(
    ets,
    function(et) {
      e <- et$e
      t <- et$t
      plot3d(e$points, type = "s", size = 1.5,
             col = e$clusters, aspect = T,
             xlab = "x", ylab = "y", zlab = "z", main = t,
             xlim = c(xmin, xmax), ylim = c(ymin, ymax), zlim = c(zmin, zmax))
    }
  )

  # Trigger display as rglwidget
  highlevel(integer())
}
