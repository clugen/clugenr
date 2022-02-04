# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

# Create parameter combinations to test
targs <- expand.grid(seed = seeds, nclu = num_clusters, len = llengths_mus,
                     lensd = llengths_sigmas)

# Loop through all parameter combinations
for (i in seq.int(1, nrow(targs))) {

  # Get current parameters
  seed <- targs[i, "seed"]
  nclu <- targs[i, "nclu"]
  len <- targs[i, "len"]
  lensd <- targs[i, "lensd"]

  # Set seed
  set.seed(seed)

  # Determine test name for current parameter set
  test_desc <- paste0("llengths: nclu=", nclu, ", len=", len, ", lensd=", lensd)

  # Perform tests for current parameter set
  test_that(test_desc, {

    # Check that the function runs without warnings
    expect_warning(lens <- llengths(nclu, len, lensd), regexp = NA)

    # Check that the output has the correct number of clusters
    expect_equal(length(lens), nclu)

    # Check that all lengths are >= 0
    expect_true(min(lens) >= 0)
  })
}
