# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

# ################################## #
# Test clugen() mandatory parameters #
# ################################## #

# Number of line directions to test
ndirs <- 1

# Create parameter combinations to test
targs <- expand.grid(seed = seeds, nd = num_dims[1:(length(num_dims) - 1)],
                     nclu = num_clusters[1:(length(num_clusters) - 1)],
                     tpts = num_points[1:(length(num_points) - 1)],
                     astd = angles_stds[1:(length(angles_stds) - 3)],
                     len = llengths_mus, len_std = llengths_sigmas,
                     lat_std = lat_stds[1:(length(lat_stds) - 1)])

# Loop through all parameter combinations
for (i in seq.int(1, nrow(targs))) {

  # Get current parameters
  seed <- targs[i, "seed"]
  nd <- targs[i, "nd"]
  nclu <- targs[i, "nclu"]
  tpts <- targs[i, "tpts"]
  astd <- targs[i, "astd"]
  len <- targs[i, "len"]
  len_std <- targs[i, "len_std"]
  lat_std <- targs[i, "lat_std"]

  # Set seed
  set.seed(seed)

  # Create combination of seed-depending parameters (line directions + clu seps)
  tsargs <- expand.grid(direc = asplit(get_vecs(ndirs, nd), 1),
                        clusep = asplit(get_clu_seps(nd), 1))

  # Loop through line directions and line centers
  for (j in seq.int(1, nrow(tsargs))) {

    # Get current line direction + line center
    direc <- tsargs[j, "direc"][[1]]
    clusep <- tsargs[j, "clusep"][[1]]

    # Determine test name for current parameter set
    test_desc <- paste0("clugen mandatory params: nd=", nd,
                        ", nclu=", nclu, ", tpts=", tpts,
                        ", astd=", astd, ", len=", len, ", len_std=", len_std,
                        ", lat_std=", lat_std,
                        ", direc=[", paste(direc, collapse = ", "),
                        "], clusep=[", paste(clusep, collapse = ", "), "]")

    # Perform tests for current parameter set
    test_that(test_desc, {

      # By default, allow_empty is false, so clugen() must be given more points
      # than clusters...
      if (tpts >= nclu) {
        # ...in which case it runs without problem
        expect_warning(r <- clugen(nd, nclu, tpts, direc, astd,
                                   clusep, len, len_std, lat_std),
                       regexp = NA)

        # Check dimensions of result variables
        expect_equal(dim(r$points), c(tpts, nd))

      } else {
        # ...otherwise an error will be thrown
        expect_error(r <- clugen(nd, nclu, tpts, direc, astd,
                                 clusep, len, len_std, lat_std),
                     paste("A total of", tpts, "points is not enough for", nclu,
                           "non-empty clusters"))

      }
    })
  }
}
