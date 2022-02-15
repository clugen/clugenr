# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

# Number of line directions to test
ndirs <- 2

# Number of line centers to test
ncts <- 2

# Create parameter combinations to test
targs <- expand.grid(seed = seeds, nd = num_dims, len = llengths_mus,
                     tpts = num_points, lat_std = lat_stds)

# Loop through all parameter combinations
for (i in seq.int(1, nrow(targs))) {

  # Get current parameters
  seed <- targs[i, "seed"]
  tpts <- targs[i, "tpts"]
  nd <- targs[i, "nd"]
  len <- targs[i, "len"]
  lat_std <- targs[i, "lat_std"]

  # Set seed
  set.seed(seed)

  # Create combination of seed-depending parameters (line directions + centers)
  tsargs <- expand.grid(direc = asplit(get_unitvecs(ndirs, nd), 1),
                        ctr = asplit(get_vecs(ncts, nd), 1))

  # Loop through line directions and line centers
  for (j in seq.int(1, nrow(tsargs))) {

    # Get current line direction + line center
    direc <- tsargs[j, "direc"][[1]]
    ctr <- tsargs[j, "ctr"][[1]]

    # Determine test name for current parameter set
    test_desc <- paste0("clupoints_n: nd=", nd,
                        ", len=", len, ", tpts=", tpts, ", lat_std=", lat_std,
                        ", direc=[", paste(direc, collapse = ", "),
                        "], ctr=[", paste(ctr, collapse = ", "), "]")

    # Perform tests for current parameter set
    test_that(test_desc, {

      # Create some random distances from center
      proj_dist_fn2ctr <- stats::runif(tpts, min = -len / 2, max = len / 2)
      proj <- points_on_line(ctr, direc, proj_dist_fn2ctr)

      # Check that the clupoints_n function runs without warnings
      expect_warning(
        pts <- clupoints_n(proj, lat_std, len, direc, ctr),
        regexp = NA)

      # Check that number of points is the same as the number of projections
      expect_equal(dim(pts), dim(proj))
    })
  }
}
