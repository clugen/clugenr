# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

#' Determine coordinates of points on a line with `center` and `direction`,
#' based on the distances from the center given in `dist_center`.
#'
#' @param center Center of the line (n-component vector).
#' @param direction Line direction (n-component unit vector).
#' @param dist_center  Distance of each point to the center of the line
#' (p-component vector, where p is the number of points).
#'
#' @return Coordinates of points on the specified line (p x n matrix).
#'
#' @export
points_on_line <- function(center, direction, dist_center) {
  rep(center, each = length(dist_center)) + dist_center %*% matrix(direction, nrow = 1)
}
