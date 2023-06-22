# Copyright (c) 2020-2023 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

is_test_mode <- function(tm) {
  identical(tolower(Sys.getenv("CLUGENR_TEST_MODE")), tolower(tm))
}

if (testthat:::on_cran() || is_test_mode("cran")) {
  # Light tests, for CRAN
  cat("Testing on CRAN\n")

  # Parameters to sweep
  seeds <- c(123)
  num_dims <- c(2)
  num_points <- c(10)
  num_clusters <- c(3)
  lat_stds <- c(10)
  llengths_mus <- c(1)
  llengths_sigmas <- c(1)
  angles_stds <- c(pi / 512)
  allow_empties <- c(TRUE)

  # Number of line directions to test
  ndirs <- 1

  # Number of line centers to test
  ncts <- 1

  # How many vectors to test?
  nvec <- 1

  # How many angles to test?
  nang <- 1

  # clumerge() specific
  t_ds_cg_n <- 1
  t_ds_ot_n <- 1
  t_no_clusters_field <- FALSE

} else if (testthat:::on_ci() || is_test_mode("ci")) {
  # Test setup for CI environments
  cat("Testing on CI\n")

  # Parameters to sweep
  seeds <- c(321)
  num_dims <- c(1, 3, 10)
  num_points <- c(1, 200)
  num_clusters <- c(1, 10)
  lat_stds <- c(0.0, 5.0)
  llengths_mus <- c(0, 10)
  llengths_sigmas <- c(0, 15)
  angles_stds <- c(0, pi / 256, pi / 2, pi)
  allow_empties <- c(TRUE, FALSE)

  # Number of line directions to test
  ndirs <- 1

  # Number of line centers to test
  ncts <- 1

  # How many vectors to test?
  nvec <- 2

  # How many angles to test?
  nang <- 2

  # clumerge() specific
  t_ds_cg_n <- 0:2
  t_ds_ot_n <- 0:1
  t_no_clusters_field <- c(FALSE, TRUE)

} else if (!is_test_mode("full")) {
  # Test setup for local tests
  cat("Testing locally\n")

  # Parameters to sweep
  seeds <- c(0, 123)
  num_dims <- c(1, 2, 3, 4, 30)
  num_points <- c(1, 10, 500)
  num_clusters <- c(1, 5, 20, 50)
  lat_stds <- c(0.0, 5.0)
  llengths_mus <- c(0, 10)
  llengths_sigmas <- c(0, 15)
  angles_stds <- c(0, pi / 4, pi / 2, pi)
  allow_empties <- c(TRUE, FALSE)

  # Number of line directions to test
  ndirs <- 2

  # Number of line centers to test
  ncts <- 2

  # How many vectors to test?
  nvec <- 4

  # How many angles to test?
  nang <- 4

  # clumerge() specific
  t_ds_cg_n <- 0:3
  t_ds_ot_n <- 0:2
  t_no_clusters_field <- c(FALSE, TRUE)

} else {
  # Heavy-duty tests if the env variable CLUGENR_TEST_FULL is set to "true"
  # Can take a long time
  cat("Testing exhaustively (slow!)\n")

  # Parameters to sweep
  seeds <- c(0, 123)
  num_dims <- c(1, 2, 3, 4, 30)
  num_points <- c(1, 10, 500, 2000, 10000)
  num_clusters <- c(1, 2, 5, 10, 100)
  lat_stds <- c(0.0, 5.0, 500)
  llengths_mus <- c(0, 10)
  llengths_sigmas <- c(0, 15)
  angles_stds <- c(0, pi / 256, pi / 4, pi / 2, pi, 2 * pi)
  allow_empties <- c(TRUE, FALSE)

  # Number of line directions to test
  ndirs <- 4

  # Number of line centers to test
  ncts <- 4

  # How many vectors to test?
  nvec <- 5

  # How many angles to test?
  nang <- 5

  # clumerge() specific
  t_ds_cg_n <- 0:4
  t_ds_ot_n <- 0:3
  t_no_clusters_field <- c(FALSE, TRUE)
}
