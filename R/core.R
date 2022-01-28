# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

#' Determine coordinates of points on a line with `center` and `direction`,
#' based on the distances from the center given in `dist_center`.
#'
#' \loadmathjax
#'
#' @param center Center of the line (\mjeqn{n}{n}-component vector).
#' @param direction Line direction (\mjeqn{n}{n}-component unit vector).
#' @param dist_center  Distance of each point to the center of the line
#' (\eqn{n}-component vector, where \mjeqn{n}{n} is the number of points).
#'
#' @return Coordinates of points on the specified line
#' (\mjeqn{p \times n}{p x n} matrix).
#'
#' @export
points_on_line <- function(center, direction, dist_center) {
  rep(center, each = length(dist_center)) + dist_center %*% matrix(direction, nrow = 1)
}
