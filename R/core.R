# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

#' Determine coordinates of points on a line
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
#' \mjdeqn{\mathbf{P}=\mathbf{1}\,\mathbf{c}^T + \mathbf{w}\mathbf{d}^T}{
#' P = 1c' + wd'}
#'
#' where \mjeqn{\mathbf{P}}{P} is the \mjeqn{p \times n}{p x n} matrix of point
#' coordinates on the line, and \mjeqn{\mathbf{1}}{1} is a \mjeqn{p \times 1}{p
#' x 1} vector with all entries equal to 1.
#'
#' @param center Center of the line (\mjseqn{n}-component vector).
#' @param direction Line direction (\mjseqn{n}-component unit vector).
#' @param dist_center  Distance of each point to the center of the line
#' (\mjseqn{n}-component vector, where \mjseqn{n} is the number of points).
#' @return Coordinates of points on the specified line
#' (\mjeqn{p \times n}{p x n} matrix).
#'
#' @export
#'
#' @examples
#'
#' points_on_line(c(5, 5), c(1, 0), seq(-4, 4, length.out=5)) # 2D, 5 points
#'
#' points_on_line(c(-2, 0, 0, 2), c(0, 0, -1, 0), c(10, -10)) # 4D, 2 points
points_on_line <- function(center, direction, dist_center) {
  rep(center, each = length(dist_center)) +
    dist_center %*% matrix(direction, nrow = 1)
}

#' Get a random unit vector orthogonal to `u`.
#'
#' @note This function is stochastic. For reproducibility set a PRNG seed with
#' [set.seed].
#'
#' @param u A unit vector.
#' @return A random unit vector orthogonal to `u`.
#'
#' @export
#'
#' @examples
#' r <- stats::runif(3)      # Get a random 3D vector
#' r <- r / norm(r, "2")     # Normalize it
#' o <- rand_ortho_vector(r) # Get a random unit vector orthogonal to r
#' r %*% o            # Check that r and o are orthogonal (result should be ~0)
rand_ortho_vector <- function(u) {

  if (length(u) == 1) {

    # If 1D, just return a random unit vector
    v <- rand_unit_vector(1)

  } else {

    # Otherwise find a random, non-parallel vector to u
    while (TRUE) {

      # Find normalized random vector
      r <- rand_unit_vector(length(u))

      # If not parallel to u we can keep it and break the loop
      if (!isTRUE(all.equal(abs(u %*% r), 1))) {
        break
      }
    }

    # Get vector orthogonal to u using 1st iteration of Gram-Schmidt process
    v <- r - c(u %*% r) / c(u %*% u) * u

    # Normalize it
    v <- v / norm(v, "2")
  }

  # Return it
  v
}

#' Get a random unit vector with `num_dims` components.
#'
#' @note This function is stochastic. For reproducibility set a PRNG seed with
#' [set.seed].
#'
#' @param num_dims Number of components in vector (i.e. vector size).
#' @return A random unit vector with `num_dims` components.
#'
#' @export
#'
#' @examples
#'
#' r <- rand_unit_vector(4)
#' norm(r, "2")
rand_unit_vector <- function(num_dims) {
  r <- stats::runif(num_dims, min = -0.5, max = 0.5)
  r / norm(r, "2")
}

#' Get a random unit vector at a given angle with another vector.
#'
#' @description
#' \loadmathjax
#' Get a random unit vector which is at `angle` radians of vector `u`.
#' Note that `u` is expected to be a unit vector itself.
#'
#' @note This function is stochastic. For reproducibility set a PRNG seed with
#' [set.seed].
#'
#' @param u Unit vector with \mjseqn{n} components.
#' @param angle Angle in radians.
#' @return Random unit vector with \mjseqn{n} components which is at `angle`
#' radians with vector `u`.
#'
#' @export
#'
#' @examples
#'
#' u <- c(1.0, 0, 0.5, -0.5)            # Define a 4D vector
#' u <- u / norm(u, "2")                # Normalize the vector
#' v <- rand_vector_at_angle(u, pi / 4) # Get a vector at 45 degrees
#' arad <- acos((u %*% v) / norm(u,"2") * norm(v, "2")) # Get angle in radians
#' arad * 180 / pi # Convert to degrees, should be close to 45 degrees
rand_vector_at_angle <- function(u, angle) {
  if (isTRUE(all.equal(abs(angle), pi / 2)) && length(u) > 1) {
    v <- rand_ortho_vector(u)
  } else if ((abs(angle) < pi / 2) && length(u) > 1) {
    v <- u + rand_ortho_vector(u) * tan(angle)
    v <- v / norm(v, "2")
  } else {
    # For |θ| > π/2 or the 1D case, simply return a random vector
    v <- rand_unit_vector(length(u))
  }
  v
}
