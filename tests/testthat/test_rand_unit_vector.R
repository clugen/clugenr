# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

# Create parameter combinations to test
targs <- expand.grid(seed = seeds, nd = num_dims)

# Loop through all parameter combinations
for (i in seq.int(1, nrow(targs))) {

  # Get current parameters
  seed <- targs[i, "seed"]
  nd <- targs[i, "nd"]

  # Set seed
  set.seed(seed)

  # Determine test name for current parameter set
  test_desc <- paste0("rand_unit_vector: nd=", nd)

  # Perform tests for current parameter set
  test_that(test_desc, {

    # Check that the function runs without warnings
    expect_warning(r <- rand_unit_vector(nd), regexp = NA)

    # Check that returned vector has the correct length
    expect_equal(length(r), nd)

    # Check that returned vector has norm == 1
    expect_equal(norm(r, "2"), 1)
  })
}
