# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

# Get n random vectors with nd-dimensions
get_vecs <- function(n, nd) {
  matrix(runif(n * nd), ncol = nd)
}

# Get n random unit vectors with nd-dimensions
get_unitvecs <- function(n, nd) {
  m <- matrix(runif(n * nd), ncol = nd)
  for (i in 1:n) {
    m[i, ] <- m[i, ] / norm(as.matrix(m[i, ]), type = "2")
  }
  m
}

# Get n angles
get_angles <- function(n) {
  stats::runif(n, min = -pi, max = pi)
}

# Get cluster offsets
get_clu_offsets <- function(nd) {
  clo <- matrix(0, 3, nd)
  clo[2, ] <- 1
  clo[3, ] <- stats::rnorm(nd, sd = 1000)
  clo
}

# Get cluster separators
get_clu_seps <- get_clu_offsets

# Get angle between two vectors, useful for checking correctness of results
# Based on AngleBetweenVectors.jl by Jeffrey Sarnoff (MIT license),
# https://github.com/JeffreySarnoff/AngleBetweenVectors.jl
# in turn based on these notes by Prof. W. Kahan, see page 15:
# https://people.eecs.berkeley.edu/~wkahan/MathH110/Cross.pdf
angle_btw <- function(v1, v2) {

  signbit <- function(x) {
    x < 0
  }

  u1 <- v1 / norm(v1, "2")
  u2 <- v2 / norm(v2, "2")

  y <- u1 - u2
  x <- u1 + u2

  a0 <- 2 * atan(norm(y, "2") / norm(x, "2"))

  if (!(signbit(a0) || signbit(pi - a0))) {
    a <- a0
  } else if (signbit(a0)) {
    a <- 0.0
  } else {
    a <- pi
  }

  a
}
