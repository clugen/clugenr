# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

# Create parameter combinations to test
targs <- expand.grid(seed = seeds, nd = num_dims, nclu = num_clusters)

# Loop through all parameter combinations
for (i in seq.int(1, nrow(targs))) {

  # Get current parameters
  seed <- targs[i, "seed"]
  nd <- targs[i, "nd"]
  nclu <- targs[i, "nclu"]

  # Set seed
  set.seed(seed)

  # Create combination of seed-depending parameters (cluster offsets + seps)
  tsargs <- expand.grid(cluoff = asplit(get_clu_offsets(nd), 1),
                        clusep = asplit(get_clu_seps(nd), 1))

  # Loop through line directions and line centers
  for (j in seq.int(1, nrow(tsargs))) {

    # Get current line direction + line center
    cluoff <- tsargs[j, "cluoff"][[1]]
    clusep <- tsargs[j, "clusep"][[1]]

    # Determine test name for current parameter set
    test_desc <- paste0("clucenters: nd=", nd, ", nclu=", nclu,
                        ", cluoff=[", paste(cluoff, collapse = ", "),
                        "], clusep=[", paste(clusep, collapse = ", "), "]")

    # Perform tests for current parameter set
    test_that(test_desc, {

      # Check that the clucenters function runs without warnings
      expect_warning(clu_ctrs <- clucenters(nclu, clusep, cluoff), regexp = NA)

      # Check that return value has the correct dimensions
      expect_equal(dim(clu_ctrs), c(nclu, nd))
    })
  }
}

