# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

#' Certifies that, given enough points, no clusters are left empty
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
#'
#' clusters <- c(3, 4, 5, 0, 0)    # A vector with some empty elements
#' clusters <- fix_empty(clusters) # Apply this function
#' clusters                        # Check that there's no more empty elements
#' # [1] 3 3 4 1 1
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
