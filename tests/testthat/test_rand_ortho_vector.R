# Copyright (c) 2020-2023 Nuno Fachada
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

  # Go through a number of randomly created unit vectors
  for (u in asplit(get_unitvecs(nvec, nd), 1)) {

    # Determine test name for current parameter set
    test_desc <- paste0("rand_ortho_vector: nd=", nd,
                        ", u=[", paste(u, collapse = ", "), "]")

    # Perform tests for current parameter set
    test_that(test_desc, {

      # Check that the function runs without warnings
      expect_warning(r <- rand_ortho_vector(u), regexp = NA)

      # Check that returned vector has the correct length
      expect_equal(length(r), nd)

      # Check that returned vector has norm == 1
      expect_equal(norm(r, "2"), 1)

      # Check that vectors u and r are orthogonal (only for nd > 1)
      if (nd > 1) {
        # The dot product of orthogonal vectors must be (approximately) zero
        expect_equal((u %*% r)[1], 0)
      }
    })
  }
}
