# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

# Create parameter combinations to test
targs <- expand.grid(seed = seeds, nclu = num_clusters, astd = angles_stds)

# Loop through all parameter combinations
for (i in seq.int(1, nrow(targs))) {

  # Get current parameters
  seed <- targs[i, "seed"]
  nclu <- targs[i, "nclu"]
  astd <- targs[i, "astd"]

  # Set seed
  set.seed(seed)

  # Determine test name for current parameter set
  test_desc <- paste0("angle_deltas: seed=", seed, ",
                      nclu=", nclu, ", astd=", astd)

  # Perform tests for current parameter set
  test_that(test_desc, {

    # Check that the angle_deltas function runs without warnings
    expect_warning(angles <- angle_deltas(nclu, astd), regexp = NA)

    # Check that return value has the correct dimensions
    expect_equal(length(angles), nclu)

    # Check that all angles are between -π/2 and π/2
    expect_true(all(abs(angles) <= pi / 2))
  })
}
