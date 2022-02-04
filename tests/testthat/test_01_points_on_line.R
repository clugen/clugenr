# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

# Number of line directions to test
ndirs <- 2

# Number of line centers to test
ncts <- 2

# Create parameter combinations to test
targs <- expand.grid(seed = seeds, nd = num_dims, len = llengths_mus,
                     tpts = num_points[num_points < 1000])

# Loop through all parameter combinations
for (i in seq.int(1, nrow(targs))) {

  # Get current parameters
  seed <- targs[i, "seed"]
  tpts <- targs[i, "tpts"]
  nd <- targs[i, "nd"]
  len <- targs[i, "len"]

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
    test_desc <- paste0("points_on_line: nd=", nd,
                        ", len=", len, ", tpts=", tpts,
                        ", direc=[", paste(direc, collapse = ", "),
                        "], ctr=[", paste(ctr, collapse = ", "), "]")

    # Perform tests for current parameter set
    test_that(test_desc, {

      # Create some random distances from center
      dist2ctr <- stats::runif(tpts, min = -len / 2, max = len / 2)

      # Check that the points_on_line function runs without warnings
      expect_warning(pts <- points_on_line(ctr, direc, dist2ctr),
                     regexp = NA)

      # Check that the dimensions agree
      expect_equal(dim(pts), c(tpts, nd))

      # Check that distance of points to the line is approximately zero
      for (pt in asplit(pts, 1)) {
        # Get distance from current point to line
        dp2l <- (pt - ctr) - c((pt - ctr) %*% direc) * direc
        d <- norm(as.matrix(dp2l, type = "2"))
        # Check that it is approximately zero
        expect_equal(d, 0)
      }
    })
  }
}
