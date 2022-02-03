# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

# Number of line directions to test
ndirs <- 1

# Number of line centers to test
ncts <- 1

# Distance from points to projections will be 10
dist_pt <- 10

# Create parameter combinations to test
targs <- expand.grid(seed = seeds, nd = num_dims, len = llengths_mus,
                     tpts = num_points[num_points < 1000], lat_std = lat_stds)

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
    test_desc <- paste0("clupoints_n_1_template: nd=", nd,
                        ", len=", len, ", tpts=", tpts, ", lat_std=", lat_std,
                        ", direc=[", paste(direc, collapse = ", "),
                        "], ctr=[", paste(ctr, collapse = ", "), "]")

    # Perform tests for current parameter set
    test_that(test_desc, {

      # Create some random distances from center
      proj_dist_fn2ctr <- len * stats::runif(tpts) - len / 2
      proj <- points_on_line(ctr, direc, proj_dist_fn2ctr)

      # Very simple dist_fn, always puts points at a distance of dist_pt
      dist_fn <- function(clu_num_points, ldisp) sample(c(-dist_pt, dist_pt),
                                                        clu_num_points,
                                                        replace = TRUE)

      # Check that the clupoints_n_1_template function runs without warnings
      expect_warning(
        pts <- clupoints_n_1_template(proj, lat_std, direc, dist_fn),
        regexp = NA)

      # Check that number of points is the same as the number of projections
      expect_equal(dim(pts), dim(proj))

      # The following checks are only for dimensionality above 1
      if (nd > 1) {
        # For each vector from projection to point...
        for (u in asplit(pts - proj, 1)) {

          # Vector should be approximately orthogonal to the cluster line
          expect_equal((direc %*% u)[1], 0)

          # Vector should have a magnitude of approximately dist_pt
          expect_equal(norm(u, "2"), dist_pt)
        }
      }
    })
  }
}
