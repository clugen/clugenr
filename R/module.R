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

#' Todo this text.
#'
#' @param num_clusters TODO.
#' @param clu_sep TODO.
#' @param clu_offset TODO.
#' @return TODO.
#'
#' @export
clucenters <- function(num_clusters, clu_sep, clu_offset) {

}

#' Todo this text.
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
#' @param num_clusters TODO.
#' @param llength TODO.
#' @param llength_disp TODO.
#' @return TODO.
#'
#' @export
llengths <- function(num_clusters, llength, llength_disp) {

}
