# Copyright (c) 2020-2023 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

# ############################################################## #
# Test clumerge with several parameters and various data sources #
# ############################################################## #

# Create parameter combinations to test
targs <- expand.grid(seed = seeds, nd = num_dims,
                     ds_cg_n = t_ds_cg_n,
                     ds_ot_n = t_ds_ot_n,
                     no_clusters_field = t_no_clusters_field)

# Loop through all parameter combinations
for (i in seq.int(1, nrow(targs))) {

  # Get current parameters
  seed <- targs[i, "seed"]
  nd <- targs[i, "nd"]
  ds_cg_n <- targs[i, "ds_cg_n"]
  ds_ot_n <- targs[i, "ds_ot_n"]
  no_clusters_field <- targs[i, "no_clusters_field"]

  # Only test if there are at least one data set to merge
  # (when there is only one, the function will just use that one)
  if (ds_cg_n + ds_ot_n == 0) next

  # Set seed
  set.seed(seed)

  # Determine test name for current parameter set
  test_desc <- paste0("clumerge general: ",
                      "seed=", seed, ", nd=", nd,
                      ", ds_cg_n=", ds_cg_n, ", ds_ot_n=", ds_ot_n,
                      ", no_clusters_field=", no_clusters_field)

  # Perform tests for current parameter set
  test_that(test_desc, {

    datasets <- list()
    tclu <- 0
    tpts <- 0

    # Create data sets with clugen()
    for (i in 1:ds_cg_n) {

      # clugen() should run without warnings
      expect_warning(ds <- clugen(nd,
                                  sample(1:10, 1),
                                  sample(1:100, 1),
                                  rnorm(nd),
                                  runif(1),
                                  runif(nd),
                                  runif(1),
                                  runif(1),
                                  runif(1),
                                  allow_empty = TRUE),
                     regexp = NA)
      tclu <- if (no_clusters_field) {
        max(tclu, max(as.vector(ds[["clusters"]], mode = "integer")))
      } else {
        tclu + length(unique(ds[["clusters"]]))
      }
      tpts <- tpts + clugenr:::gdim(ds[["points"]])[1]
      datasets <- append(datasets, list(ds))
    }

    # Create non-clugen() data sets
    for (i in 1:ds_ot_n) {
      npts <- sample(1:100, 1)
      nclu <- sample(1:min(3, npts), 1)
      ds <- list(points = matrix(rnorm(npts * nd), ncol = nd),
                 clusters = factor(sample(1:nclu, npts, replace = TRUE)))
      tclu <- if (no_clusters_field) {
        max(tclu, max(as.vector(ds[["clusters"]], mode = "integer")))
      } else {
        tclu + length(unique(ds[["clusters"]]))
      }
      tpts <- tpts + clugenr:::gdim(ds[["points"]])[1]
      datasets <- append(datasets, list(ds))
    }

    clufield <- if (no_clusters_field) NA else "clusters"

    # Check that clumerge() is able to merge data sets without warnings
    args <- c(datasets, list(clusters_field = clufield))
    expect_warning(mds <- do.call(clumerge, args), regexp = NA)

    # Check that the number of points and clusters is correct
    expect_equal(clugenr:::gdim(mds$points), c(tpts, nd))
    expect_equal(max(as.vector(mds$clusters, mode = "integer")), tclu)
    expect_equal(typeof(mds$clusters), "integer")
    expect_true(is.factor(mds$clusters))
  })
}

# ############################################################# #
# Test clumerge with data from clugen() and merging more fields #
# ############################################################# #

# Create parameter combinations to test
targs <- expand.grid(seed = seeds, nd = num_dims, ds_cg_n = t_ds_cg_n)

# Loop through all parameter combinations
for (i in seq.int(1, nrow(targs))) {

  # Get current parameters
  seed <- targs[i, "seed"]
  nd <- targs[i, "nd"]
  ds_cg_n <- targs[i, "ds_cg_n"] + 1

  # Set seed
  set.seed(seed)

  # Determine test name for current parameter set
  test_desc <- paste0("clumerge clugen more fields: ",
                      "seed=", seed, ", nd=", nd, ", ds_cg_n=", ds_cg_n)

  # Perform tests for current parameter set
  test_that(test_desc, {

    datasets <- list()
    tclu <- 0
    tclu_i <- 0
    tpts <- 0

    for (i in 1:ds_cg_n) {
      # clugen() should run without warnings
      expect_warning(ds <- clugen(nd,
                                  sample(1:10, 1),
                                  sample(1:100, 1),
                                  rnorm(nd),
                                  runif(1),
                                  runif(nd),
                                  runif(1),
                                  runif(1),
                                  runif(1),
                                  allow_empty = TRUE),
                     regexp = NA)

      tclu <- tclu + length(unique(ds$clusters))
      tpts <- tpts + clugenr:::gdim(ds$points)[1]
      tclu_i <- tclu_i + clugenr:::gdim(ds$sizes)[1]
      datasets <- append(datasets, list(ds))
    }

    # Check that clumerge() is able to merge data set fields related to points
    # without warnings
    args <- c(datasets, list(fields = c("points", "clusters", "projections")))
    expect_warning(mds <- do.call(clumerge, args), regexp = NA)

    # Check that the number of clusters and points is correct
    expect_equal(clugenr:::gdim(mds$points), c(tpts, nd))
    expect_equal(clugenr:::gdim(mds$projections), c(tpts, nd))
    expect_equal(max(as.vector(mds$clusters, mode = "integer")), tclu)
    expect_equal(typeof(mds$clusters), "integer")
    expect_true(is.factor(mds$clusters))

    # Check that clumerge() is able to merge data set fields related to clusters
    # without warnings
    args <- c(datasets,
              list(fields = c("sizes", "centers",
                              "directions", "angles", "lengths")),
              list(clusters_field = NA))
    expect_warning(mds <- do.call(clumerge, args), regexp = NA)

    # Check that the cluster-related fields have the correct sizes
    expect_equal(clugenr:::gdim(mds$sizes), c(tclu_i, 1))
    expect_equal(clugenr:::gdim(mds$centers), c(tclu_i, nd))
    expect_equal(clugenr:::gdim(mds$directions), c(tclu_i, nd))
    expect_equal(clugenr:::gdim(mds$angles), c(tclu_i, 1))
    expect_equal(clugenr:::gdim(mds$lengths), c(tclu_i, 1))
  })
}

# ################################################### #
# Test that clumerge() raises the expected exceptions #
# ################################################### #

for (seed in seeds) {

  # Set seed
  set.seed(seed)

  test_that(paste0("clumerge exceptions: seed=", seed), {

    # Data item does not contain required field `unknown`
    nd <- 3
    npts <- sample(10:100, 1)
    ds <- list(points = matrix(rnorm(npts * nd), ncol = nd),
               clusters = sample(1:5, npts, replace = TRUE))
    expect_error(clumerge(ds, fields = c("clusters", "unknown")),
                 regexp = "Data item does not contain required field `unknown`",
                 fixed = TRUE)

    # "`clusters_field` must contain integer types
    nd <- 4
    npts <- sample(10:100, 1)
    ds <- list(points = matrix(rnorm(npts * nd), ncol = nd),
               clusters = rep(1, npts))
    expect_error(clumerge(ds),
                 regexp = "`clusters` must contain integer types",
                 fixed = TRUE)

    # Data item contains fields with different sizes (npts != npts / 2)
    nd <- 2
    npts <- sample(10:100, 1)
    ds <- list(points = matrix(rnorm(npts * nd), ncol = nd),
               clusters = sample(1:5, npts %/% 2, replace = TRUE))
    expect_error(clumerge(ds),
                 regexp = paste0("Data item contains fields with different",
                                 " sizes \\([0-9]+ != [0-9]+\\)"))

    # Dimension mismatch in field `points`
    nd1 <- 2
    nd2 <- 3
    npts <- sample(10:100, 1)
    ds1 <- list(points = matrix(rnorm(npts * nd1), ncol = nd1),
                clusters = sample(1:5, npts, replace = TRUE))
    ds2 <- list(points = matrix(rnorm(npts * nd2), ncol = nd2),
                clusters = sample(1:5, npts, replace = TRUE))
    expect_error(clumerge(ds1, ds2),
                 regexp = "Dimension mismatch in field `points`",
                 fixed = TRUE)

    # `clusters_field` has more than one dimension
    nd <- 2
    npts <- sample(10:100, 1)
    ds <- list(points = matrix(rnorm(npts * nd), ncol = nd),
               clusters = matrix(sample(1:5, npts * nd, replace = TRUE),
                                 ncol = nd))
    expect_error(clumerge(ds),
                 regexp = "Clusters field `clusters` has more than one dimension",
                 fixed = TRUE)

    # Factor mismatch
    nd <- 5
    npts1 <- sample(10:100, 1)
    npts2 <- sample(10:100, 1)
    ds1 <- list(points = matrix(rnorm(npts1 * nd), ncol = nd),
                clusters = sample(1:5, npts1, replace = TRUE))
    ds2 <- list(points = matrix(rnorm(npts2 * nd), ncol = nd),
                clusters = factor(sample(1:5, npts2, replace = TRUE)))
    expect_error(clumerge(ds1, ds2),
                 regexp = "Factor mismatch in field `clusters`",
                 fixed = TRUE)

    # Confirm that type promotion happens and does not cause problems
    nd <- 3
    npts <- sample(10:100, 1)
    ds1 <- list(points = matrix(rnorm(npts * nd), ncol = nd),
                clusters = as.vector(sample(1:5, npts, replace = TRUE),
                                     mode = "double"))
    ds2 <- list(points = matrix(rnorm(npts * nd), ncol = nd),
                clusters = sample(1:5, npts, replace = TRUE))
    expect_warning(mds <- clumerge(ds1, ds2, clusters_field = NA), regexp = NA)
    expect_type(mds$clusters, "double")
  })
}
