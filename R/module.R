# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

#' Get angles between average cluster direction and cluster-supporting lines
#'
#' @description
#' \loadmathjax
#' Determine the angles between the average cluster direction and the
#' cluster-supporting lines. These angles are obtained from a wrapped normal
#' distribution (\mjeqn{\mu=0}{μ=0}, \mjeqn{\sigma=}{σ=}`angle_disp`) with
#' support in the interval \mjeqn{\left\[-\pi/2,\pi/2\right\]}{\[-π/2,π/2\]}.
#' Note this is different from the standard wrapped normal distribution, the
#' support of which is given by the interval
#' \mjeqn{\left\[-\pi,\pi\right\]}{\[-π,π\]}.
#'
#' @note This function is stochastic. For reproducibility set a PRNG seed with
#' [set.seed].
#'
#' @param num_clusters Number of clusters.
#' @param angle_disp Angle dispersion, in radians.
#' @return Angles between the average cluster direction and the
#' cluster-supporting lines, given in radians in the interval
#' \mjeqn{\left\[-\pi/2,\pi/2\right\]}{\[-π/2,π/2\]}
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' arad <- angle_deltas(4, pi / 8) # Angle dispersion of 22.5 degrees
#' arad                            # What angles deltas did we get?
#' # [1] -0.22009827 -0.09039049  0.61210332  0.02768858
#' arad * 180 / pi                  #  Show angle deltas in degrees
#' # [1] -12.610702  -5.178994  35.070937   1.586439
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
#' #            [,1]     [,2]
#' # [1,]  -8.969561 42.65221
#' # [2,] -10.644303 46.71536
#' # [3,] -73.560159 45.23540
clucenters <- function(num_clusters, clu_sep, clu_offset) {

  # Obtain a num_clusters x num_dims matrix of uniformly distributed values
  # between -0.5 and 0.5 representing the relative cluster centers
  ctr_rel <- stats::runif(num_clusters * length(clu_sep), min = -0.5, max = 0.5)
  ctr_rel <- matrix(ctr_rel, nrow = num_clusters)

  # Convert clu_sep to diagonal matrix; if it's a one-element vector just use
  # as.matrix()
  clu_sep <- if (length(clu_sep) == 1) as.matrix(clu_sep) else diag(clu_sep)

  num_clusters * ctr_rel %*% clu_sep + rep(clu_offset, each = num_clusters)
}

#' Todo this text.
#'
#' @note This function is stochastic. For reproducibility set a PRNG seed with
#' [set.seed].
#'
#' @param projs TODO.
#' @param lat_disp TODO.
#' @param line_len TODO.
#' @param clu_dir TODO.
#' @param clu_ctr TODO.
#' @return TODO.
#'
#' @export
clupoints_n_1 <- function(projs, lat_disp, line_len, clu_dir, clu_ctr) {

}

#' Todo this text.
#'
#' @note This function is stochastic. For reproducibility set a PRNG seed with
#' [set.seed].
#'
#' @param projs TODO.
#' @param lat_disp TODO.
#' @param line_len TODO.
#' @param clu_dir TODO.
#' @param clu_ctr TODO.
#' @return TODO.
#'
#' @export
clupoints_n <- function(projs, lat_disp, line_len, clu_dir, clu_ctr) {

}

#' Todo this text.
#'
#' @note This function is stochastic. For reproducibility set a PRNG seed with
#' [set.seed].
#'
#' @param num_clusters TODO.
#' @param num_points TODO.
#' @param allow_empty TODO.
#' @return TODO.
#'
#' @export
clusizes <- function(num_clusters, num_points, allow_empty) {

}

#' Todo this text.
#'
#' @note This function is stochastic. For reproducibility set a PRNG seed with
#' [set.seed].
#'
#' @param num_clusters TODO.
#' @param llength TODO.
#' @param llength_disp TODO.
#' @return TODO.
#'
#' @export
llengths <- function(num_clusters, llength, llength_disp) {

}
