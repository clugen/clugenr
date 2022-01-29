# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

#' Determine coordinates of points on a line.
#'
#' @description
#' \loadmathjax
#' Determine coordinates of points on a line with `center` and `direction`,
#' based on the distances from the center given in `dist_center`.
#'
#' This works by using the vector formulation of the line equation assuming
#' `direction` is a \mjseqn{n}-dimensional unit vector. In other words,
#' considering \mjeqn{\mathbf{d}=}{d =} `as.matrix(direction)` (\mjeqn{n \times
#' 1}{n x 1} vector), \mjeqn{\mathbf{c}=}{c =} `as.matrix(center)` (\mjeqn{n
#' \times 1}{n x 1} vector), and \mjeqn{\mathbf{w}=}{w =}
#' `as.matrix(dist_center)` (\mjeqn{p \times 1}{p x 1} vector), the coordinates
#' of points on the line are given by:
#'
#' \mjdeqn{\mathbf{P}=\mathbf{1}\,\mathbf{c}^T + \mathbf{w}\mathbf{d}^T}{P = 1c' + wd'}
#'
#' where \mjeqn{\mathbf{P}}{P} is the \mjeqn{p \times n}{p x n} matrix of point
#' coordinates on the line, and \mjeqn{\mathbf{1}}{1} is a \mjeqn{p \times 1}{p
#' x 1} vector with all entries equal to 1.
#'
#' @param center Center of the line (\mjseqn{n}-component vector).
#' @param direction Line direction (\mjseqn{n}-component unit vector).
#' @param dist_center  Distance of each point to the center of the line
#' (\mjseqn{n}-component vector, where \mjseqn{n} is the number of points).
#'
#' @return Coordinates of points on the specified line
#' (\mjeqn{p \times n}{p x n} matrix).
#'
#' @export
points_on_line <- function(center, direction, dist_center) {
  rep(center, each = length(dist_center)) + dist_center %*% matrix(direction, nrow = 1)
}
