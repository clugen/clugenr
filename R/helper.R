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
fix_empty <- function(clu_num_points, allow_empty) {

}
