# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

#' Angle between two \mjseqn{n}-dimensional vectors.
#'
#' @description
#' \loadmathjax
#' Typically, the angle between two vectors `v1` and `v2` can be obtained with:
#'
#' ```R
#' acos((v1 %*% v2) / (norm(v1, "2") * norm(v2, "2")))
#' ```
#' However, this approach is numerically unstable. The version provided here is
#' numerically stable and based on the [Angle Between
#' Vectors](https://github.com/JeffreySarnoff/AngleBetweenVectors.jl)
#' Julia package by Jeffrey Sarnoff (MIT license), implementing an algorithm
#' provided by Prof. W. Kahan in
#' [these notes](https://people.eecs.berkeley.edu/~wkahan/MathH110/Cross.pdf)
#' (see page 15).
#'
#' @param v1 First vector.
#' @param v2 Second vector.
#' @return Angle between `v1` and `v2` in radians.
#'
#' @export
#'
#' @examples
#'
#' angle_btw(c(1.0, 1.0, 1.0, 1.0), c(1.0, 0.0, 0.0, 0.0)) * 180 / pi
angle_btw <- function(v1, v2) {

  signbit <- function(x) {
    x < 0
  }

  u1 <- v1 / norm(v1, "2")
  u2 <- v2 / norm(v2, "2")

  y <- u1 - u2
  x <- u1 + u2

  a0 <- 2 * atan(norm(y, "2") / norm(x, "2"))

  if (!(signbit(a0) || signbit(pi - a0))) {
    a <- a0
  } else if (signbit(a0)) {
    a <- 0.0
  } else {
    a <- pi
  }

  a
}

#' Create points from their projections on a cluster-supporting line
#'
#' @description
#' \loadmathjax
#' Generate points from their \mjseqn{n}-dimensional projections on a
#' cluster-supporting line, placing each point on a hyperplane orthogonal to
#' that line and centered at the point's projection. The function specified in
#' `dist_fn` is used to perform the actual placement.
#'
#' @details
#' This function is used internally by [clupoints_n_1] and may be useful for
#' constructing user-defined final point placement strategies for the
#' `point_dist_fn` parameter of the main [clugen] function.
#'
#' @note This function is stochastic. For reproducibility set a PRNG seed with
#' [set.seed].
#'
#' @param projs Point projections on the cluster-supporting line
#' (\mjeqn{p \times n}{p x n} matrix).
#' @param lat_disp Dispersion of points from their projection.
#' @param clu_dir Direction of the cluster-supporting line (unit vector).
#' @param dist_fn Function to place points on a second line, orthogonal to the
#' first.
#' @return Generated points (\mjeqn{p \times n}{p x n} matrix).
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' ctr <- c(0, 0)
#' dir <- c(1, 0)
#' pdist <- c(-0.5, -0.2, 0.1, 0.3)
#' proj <- points_on_line(ctr, dir, pdist)
#' clupoints_n_1_template(proj, 0, dir, function(p, l) stats::runif(p))
clupoints_n_1_template <- function(projs, lat_disp, clu_dir, dist_fn) {

  # Number of dimensions
  num_dims <- length(clu_dir)

  # Number of points in this cluster
  clu_num_points <- dim(projs)[1]

  # Get distances from points to their projections on the line
  points_dist <- dist_fn(clu_num_points, lat_disp)

  # Get normalized vectors, orthogonal to the current line, for each point
  orth_vecs <- matrix(0, clu_num_points, num_dims)

  for (j in 1:clu_num_points) {
    orth_vecs[j, ] <- rand_ortho_vector(clu_dir)
  }

  # Set vector magnitudes
  orth_vecs <- abs(points_dist) * orth_vecs

  # Add perpendicular vectors to point projections on the line,
  # yielding final cluster points
  points <- projs + orth_vecs

  points
}

#' Certify that, given enough points, no clusters are left empty
#'
#' @description
#' \loadmathjax
#' Certifies that, given enough points, no clusters are left empty. This is done
#' by removing a point from the largest cluster and adding it to an empty
#' cluster while there are empty clusters. If the total number of points is
#' smaller than the number of clusters (or if the `allow_empty` parameter is set
#' to `TRUE`), this function does nothing.
#'
#' @details
#' This function is used internally by [clusizes] and might be useful for custom
#' cluster sizing implementations given as the `clusizes_fn` parameter of the
#' main [clugen] function.
#'
#' @param clu_num_points Number of points in each cluster (vector of size
#' \mjseqn{c}), where \mjseqn{c} is the number of clusters.
#' @param allow_empty Allow empty clusters?
#' @return Number of points in each cluster, after being fixed by this function
#' (vector of size \mjseqn{c}).
#'
#' @export
#'
#' @examples
#' clusters <- c(3, 4, 5, 0, 0)    # A vector with some empty elements
#' clusters <- fix_empty(clusters) # Apply this function
#' clusters                        # Check that there's no more empty elements
fix_empty <- function(clu_num_points, allow_empty = FALSE) {

  # If the allow_empty parameter is set to true, don't change the number of
  # points in each cluster; this is useful for quick `clusizes_fn` one-liners
  if (!allow_empty) {

    # Find empty clusters
    empty_clusts <- which(clu_num_points == 0)

    # If there are empty clusters and enough points for all clusters...
    if (length(empty_clusts) > 0 &&
        sum(clu_num_points) >= length(clu_num_points)) {

      # Go through the empty clusters...
      for (i0 in empty_clusts) {

        # ...get a point from the largest cluster and assign it to the
        # current empty cluster
        imax <- which.max(clu_num_points)
        clu_num_points[imax] <- clu_num_points[imax] - 1
        clu_num_points[i0] <- 1
      }
    }
  }

  clu_num_points
}

#' Certify that array values add up to a specific total
#'
#' @description
#' \loadmathjax
#' Certifies that the values in the `clu_num_points` array, i.e. the number of
#' points in each cluster, add up to `num_points`. If this is not the case, the
#' `clu_num_points` array is modified in-place, incrementing the value
#' corresponding to the smallest cluster while
#' `sum(clu_num_points) < num_points`, or decrementing the value corresponding
#' to the largest cluster while `sum(clu_num_points) > num_points`.
#'
#' @details
#' This function is used internally by [clusizes] and might be useful for
#' custom cluster sizing implementations given as the `clusizes_fn` parameter of
#' the main [clugen] function.
#'
#' @param clu_num_points Number of points in each cluster (vector of size
#' \mjseqn{c}), where \mjseqn{c} is the number of clusters.
#' @param num_points The expected total number of points.
#' @return Number of points in each cluster, after being fixed by this function.
#'
#' @export
#'
#' @examples
#' clusters <- c(1, 6, 3)                   # 10 total points
#' clusters <- fix_num_points(clusters, 12) # But we want 12 total points
#' clusters                                 # Check that we now have 12 points
fix_num_points <- function(clu_num_points, num_points) {

  while (sum(clu_num_points) < num_points) {
    imin <- which.min(clu_num_points)
    clu_num_points[imin] <- clu_num_points[imin] + 1
  }
  while (sum(clu_num_points) > num_points) {
    imax <- which.max(clu_num_points)
    clu_num_points[imax] <- clu_num_points[imax] - 1
  }

  clu_num_points
}
