# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

# Create parameter combinations to test
targs <- expand.grid(seed = seeds, nclu = num_clusters, tpts = num_points,
                     ae = allow_empties)

# Loop through all parameter combinations
for (i in seq.int(1, nrow(targs))) {

  # Get current parameters
  seed <- targs[i, "seed"]
  nclu <- targs[i, "nclu"]
  tpts <- targs[i, "tpts"]
  ae <- targs[i, "ae"]

  # Don't test if number of points is less than number of clusters and we don't
  # allow empty clusters
  if (!ae && tpts < nclu) {
    next
  }

  # Set seed
  set.seed(seed)

  # Determine test name for current parameter set
  test_desc <- paste0("clusizes: nclu=", nclu, ", tpts=", tpts, ", ae=", ae)

  # Perform tests for current parameter set
  test_that(test_desc, {

    # Check that the function runs without warnings
    expect_warning(clu_sizes <- clusizes(nclu, tpts, ae), regexp = NA)

    # Check that the output has the correct number of clusters
    expect_equal(length(clu_sizes), nclu)

    # Check that the total number of points is correct
    expect_equal(sum(clu_sizes), tpts)

    # # If empty clusters are not allowed, check that all of them have points
    if (!ae) {
      expect_true(min(clu_sizes) > 0)
    }
  })
}
