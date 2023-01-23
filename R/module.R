# Copyright (c) 2020-2023 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

#' Get angles between average cluster direction and cluster-supporting lines
#'
#' @description
#' \loadmathjax
#' Determine the angles between the average cluster direction and the
#' cluster-supporting lines. These angles are obtained from a wrapped normal
#' distribution (\mjeqn{\mu=0}{μ=0}, \mjeqn{\sigma=}{σ=} `angle_disp` ) with
#' support in the interval \mjeqn{\left[-\pi/2,\pi/2\right]}{[-π/2, π/2]}.
#' Note this is different from the standard wrapped normal distribution, the
#' support of which is given by the interval
#' \mjeqn{\left[-\pi,\pi\right]}{[-π, π]}.
#'
#' @note This function is stochastic. For reproducibility set a PRNG seed with
#' [set.seed].
#'
#' @param num_clusters Number of clusters.
#' @param angle_disp Angle dispersion, in radians.
#' @return Angles between the average cluster direction and the
#' cluster-supporting lines, given in radians in the interval
#' \mjeqn{\left[-\pi/2,\pi/2\right]}{[-π/2, π/2]}
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' arad <- angle_deltas(4, pi / 8) # Angle dispersion of 22.5 degrees
#' arad                            # What angles deltas did we get?
#' arad * 180 / pi                 #  Show angle deltas in degrees
angle_deltas <- function(num_clusters, angle_disp) {

  # Get random angle differences using the normal distribution
  angles <- stats::rnorm(num_clusters, sd = angle_disp)

  # Reduce angle differences to the interval [-π, π]
  angles <- atan2(sin(angles), cos(angles))

  # Make sure angle differences are within interval [-π/2, π/2]
  sapply(angles, function(a) if (abs(a) > pi / 2) a - sign(a) * pi / 2 else a)
}

#' Determine cluster centers using the uniform distribution
#'
#' @description
#' \loadmathjax
#' Determine cluster centers using the uniform distribution, taking into account
#' the number of clusters (`num_clusters`) and the average cluster separation
#' (`clu_sep`).
#'
#' More specifically, let \mjseqn{c=}`num_clusters`,
#' \mjeqn{\mathbf{s}=}{s=}`clu_sep`, \mjeqn{\mathbf{o}=}{o=}`clu_offset`,
#' \mjseqn{n=}`length(clu_sep)` (i.e., number of dimensions). Cluster centers
#' are obtained according to the following equation:
#'
#' \mjdeqn{\mathbf{C}=c\mathbf{U}\cdot\operatorname{diag}(\mathbf{s}) +
#' \mathbf{1}\,\mathbf{o}^T}{C=cU.diag(s) + 1o'}
#'
#' where \mjeqn{\mathbf{C}}{C} is the \mjeqn{c \times n}{c x n} matrix of
#' cluster centers, \mjeqn{\mathbf{U}}{U} is an \mjeqn{c \times n}{c x n} matrix
#' of random values drawn from the uniform distribution between -0.5 and 0.5,
#' and \mjeqn{\mathbf{1}}{1} is an \mjeqn{c \times 1}{c x 1} vector with all
#' entries equal to 1.
#'
#' @note This function is stochastic. For reproducibility set a PRNG seed with
#' [set.seed].
#'
#' @param num_clusters Number of clusters.
#' @param clu_sep Average cluster separation (\mjeqn{n \times 1}{n x 1} vector).
#' @param clu_offset Cluster offsets (\mjeqn{n \times 1}{n x 1} vector).
#' @return A \mjeqn{c \times n}{c x n} matrix containing the cluster centers.
#'
#' @export
#'
#' @examples
#' set.seed(321)
#' clucenters(3, c(30, 10), c(-50,50))
clucenters <- function(num_clusters, clu_sep, clu_offset) {

  # Obtain a num_clusters x num_dims matrix of uniformly distributed values
  # between -0.5 and 0.5 representing the relative cluster centers
  ctr_rel <- stats::runif(num_clusters * length(clu_sep), min = -0.5, max = 0.5)
  ctr_rel <- matrix(ctr_rel, nrow = num_clusters)

  # Convert clu_sep to diagonal matrix; if it's a one-element vector just
  # use as.matrix()
  clu_sep <- if (length(clu_sep) == 1) as.matrix(clu_sep) else diag(clu_sep)

  num_clusters * ctr_rel %*% clu_sep + rep(clu_offset, each = num_clusters)
}

#' Create points from their projections on a cluster-supporting line
#'
#' @description
#' \loadmathjax
#' Each point is placed on a hyperplane orthogonal to that line and centered at
#' the point's projection, using the normal distribution (\mjeqn{\mu=0}{μ=0},
#' \mjeqn{\sigma=}{σ=} `lat_disp` ).
#'
#' @details
#' This function's main intended use is by the main [clugen] function,
#' generating the final points when the `point_dist_fn` parameter is set to
#' `"n-1"`.
#'
#' @note This function is stochastic. For reproducibility set a PRNG seed with
#' [set.seed].
#'
#' @param projs Point projections on the cluster-supporting line
#' (\mjeqn{p \times n}{p x n} matrix).
#' @param lat_disp Standard deviation for the normal distribution, i.e., cluster
#' lateral dispersion.
#' @param line_len Length of cluster-supporting line (ignored).
#' @param clu_dir Direction of the cluster-supporting line.
#' @param clu_ctr Center position of the cluster-supporting line (ignored).
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
#' clupoints_n_1(proj, 0.1, NA, dir, NA)
clupoints_n_1 <- function(projs, lat_disp, line_len, clu_dir, clu_ctr) {

  # Define function to get distances from points to their projections on the
  # line (i.e., using the normal distribution)
  dist_fn <- function(clu_npoints, ldisp) stats::rnorm(clu_npoints, sd = ldisp)

  # Use clupoints_n_1_template() to do the heavy lifting
  clupoints_n_1_template(projs, lat_disp, clu_dir, dist_fn)
}

#' Create points from their projections on a cluster-supporting line
#'
#' @description
#' \loadmathjax
#' Each point is placed around its projection using the normal distribution
#' (\mjeqn{\mu=0}{μ=0}, \mjeqn{\sigma=}{σ=} `lat_disp` ).
#'
#' @details
#' This function's main intended use is by the main [clugen] function,
#' generating the final points when the `point_dist_fn` parameter is set to
#' `"n"`.
#'
#' @note This function is stochastic. For reproducibility set a PRNG seed with
#' [set.seed].
#'
#' @param projs Point projections on the cluster-supporting line
#' (\mjeqn{p \times n}{p x n} matrix).
#' @param lat_disp Standard deviation for the normal distribution, i.e., cluster
#' lateral dispersion.
#' @param line_len Length of cluster-supporting line (ignored).
#' @param clu_dir Direction of the cluster-supporting line.
#' @param clu_ctr Center position of the cluster-supporting line (ignored).
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
#' clupoints_n(proj, 0.01, NA, dir, NA)
clupoints_n <- function(projs, lat_disp, line_len, clu_dir, clu_ctr) {

  # Number of dimensions
  num_dims <- length(clu_dir)

  # Number of points in this cluster
  clu_num_points <- nrow(projs)

  # Get random displacement vectors for each point projection
  displ <- matrix(stats::rnorm(clu_num_points * num_dims, sd = lat_disp),
                  nrow = clu_num_points)

  # Add displacement vectors to each point projection and return the result
  projs + displ
}

#' Determine cluster sizes, i.e., the number of points in each cluster
#'
#' @description
#' \loadmathjax
#' Cluster sizes are determined using the normal distribution
#' (\mjeqn{\mu=}{μ=} `num_points` \mjseqn{/} `num_clusters`,
#' \mjeqn{\sigma=\mu/3}{σ=μ/3}), and then assuring that the final cluster sizes
#' add up to `num_points` via the [fix_num_points] function.
#'
#' @note This function is stochastic. For reproducibility set a PRNG seed with
#' [set.seed].
#'
#' @param num_clusters Number of clusters.
#' @param num_points Total number of points.
#' @param allow_empty Allow empty clusters?
#' @return Number of points in each cluster (vector of length `num_clusters`).
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' sizes <- clusizes(4, 1000, TRUE)
#' sizes
#' sum(sizes)
clusizes <- function(num_clusters, num_points, allow_empty) {

    # Determine number of points in each cluster using the normal distribution

    # Consider the mean an equal division of points between clusters
    mean <- num_points / num_clusters
    # The standard deviation is such that the interval [0, 2 * mean] will
    # contain ≈99.7% of cluster sizes
    std <- mean / 3

    # Determine points with the normal distribution
    clu_num_points <- stats::rnorm(num_clusters, mean = mean, sd = std)

    # Set negative values to zero
    clu_num_points <- sapply(clu_num_points, function(x) if (x > 0) x else 0)

    # Fix imbalances, so that num_points is respected
    if (sum(clu_num_points) > 0) { # Be careful not to divide by zero
      clu_num_points <- num_points / sum(clu_num_points) * clu_num_points
    }

    # Round the real values to integers since a cluster sizes is represented by
    # an integer
    clu_num_points <- as.integer(round(clu_num_points))

    # Make sure total points is respected, which may not be the case at this
    # time due to rounding
    clu_num_points <- fix_num_points(clu_num_points, num_points)

    # If empty clusters are not allowed, make sure there aren't any
    if (!allow_empty) {
      clu_num_points <- fix_empty(clu_num_points, allow_empty)
    }

    clu_num_points
}

#' Determine length of cluster-supporting lines
#'
#' @description
#' \loadmathjax
#' Line lengths are determined using the folded normal distribution
#' (\mjeqn{\mu=}{μ=} `llength`, \mjeqn{\sigma=}{σ=} `llength_disp` ).
#'
#' @note This function is stochastic. For reproducibility set a PRNG seed with
#' [set.seed].
#'
#' @param num_clusters Number of clusters.
#' @param llength Average line length.
#' @param llength_disp Line length dispersion.
#' @return Lengths of cluster-supporting lines (vector of size `num_clusters`).
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' llengths(4, 20, 3.5)
llengths <- function(num_clusters, llength, llength_disp) {
  abs(stats::rnorm(num_clusters, mean = llength, sd = llength_disp))
}
