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
