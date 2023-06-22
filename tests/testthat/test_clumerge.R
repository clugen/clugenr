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
