# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

#' Generate multidimensional clusters.
#'
#' @description
#' \loadmathjax
#' TODO
#'
#' @details
#' TODO
#'
#' @note This function is stochastic. For reproducibility set a PRNG seed with
#' [set.seed].
#'
#' @param num_dims TODO.
#' @param num_clusters TODO.
#' @param num_points TODO.
#' @param direction TODO.
#' @param angle_disp TODO.
#' @param cluster_sep TODO.
#' @param llength TODO.
#' @param llength_disp TODO.
#' @param lateral_disp TODO.
#' @param allow_empty  TODO.
#' @param cluster_offset  TODO.
#' @param proj_dist_fn TODO.
#' @param point_dist_fn TODO.
#' @param clusizes_fn TODO.
#' @param clucenters_fn TODO.
#' @param llengths_fn TODO.
#' @param angle_deltas_fn TODO.
#' @return TODO.
#'
#' @export
#' @examples
#' clugen(3, 5, 1000, c(2,3,4), 0.1, c(1,3,4), 10, 0.1, 0.1)
clugen <- function(num_dims, num_clusters, num_points, direction, angle_disp,
  cluster_sep, llength, llength_disp, lateral_disp,
  allow_empty = FALSE, cluster_offset = NA, proj_dist_fn = "norm",
  point_dist_fn = "n-1", clusizes_fn = clusizes, clucenters_fn = clucenters,
  llengths_fn = llengths, angle_deltas_fn = angle_deltas) {

  # Check that number of dimensions is > 0
  if (num_dims < 1) {
    stop("Number of dimensions, `num_dims`, must be > 0")
  }

  # Check that number of clusters is > 0
  if (num_clusters < 1) {
    stop("Number of clusters, `num_clust`, must be > 0")
  }

  # Check that direction vector has magnitude > 0
  if (isTRUE(all.equal(norm(direction, "2"), 0))) {
    stop("`direction` must have magnitude > 0")
  }

  # Check that direction has num_dims dimensions
  if (length(direction) != num_dims) {
    stop("Length of `direction` must be equal to `num_dims` (",
         length(direction), " != ", num_dims, ")")
  }

  # If allow_empty is false, make sure there are enough points to distribute
  # by the clusters
  if (!allow_empty && num_points < num_clusters) {
    stop("A total of ", num_points, " points is not enough for ",
         num_clusters, " non-empty clusters")
  }


  # Check that cluster_sep has num_dims dimensions
  if (length(cluster_sep) != num_dims) {
    stop("Length of `cluster_sep` must be equal to `num_dims` (",
      length(cluster_sep), " !=  ", num_dims, ")")
  }

  # If given, cluster_offset must have the correct number of dimensions,
  # if not given then it will be a num_dims x 1 vector of zeros
  if (length(cluster_offset) == 1 && is.na(cluster_offset)) {
    cluster_offset <- vector(mode = "integer", length = num_dims)
  } else if (length(cluster_offset) != num_dims) {
    stop("Length of `cluster_offset` must be equal to `num_dims` (",
      length(cluster_offset), " != ", num_dims, ")")
  }

}
