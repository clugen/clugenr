# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

# Number of line directions to test
ndirs <- 1

# ################################## #
# Test clugen() mandatory parameters #
# ################################## #

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
        expect_equal(length(r$point_clusters), tpts)
        expect_equal(dim(r$point_projections), c(tpts, nd))
        expect_equal(length(r$cluster_sizes), nclu)
        expect_equal(dim(r$cluster_centers), c(nclu, nd))
        expect_equal(dim(r$cluster_directions), c(nclu, nd))
        expect_equal(length(r$cluster_angles), nclu)
        expect_equal(length(r$cluster_lengths), nclu)

        # Check point cluster indexes
        expect_equal(unique(r$point_clusters), 1:nclu)

        # Check total points
        expect_equal(sum(r$cluster_sizes), tpts)

        # Check that cluster directions have the correct angles with the main
        # direction
        if (nd > 1) {
          for (i in 1:nclu) {
            expect_equal(angle_btw(direc, r$cluster_directions[i, ]),
                         abs(r$cluster_angles[i]))
          }
        }

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

# ################################# #
# Test clugen() optional parameters #
# ################################# #

# Valid arguments
nclu <- 7
tpts <- 500
astd <- pi/256
len_mu <- 9
len_std <- 1.2
lat_std <- 2

# Alternative functions used for testing
ptdist_equi <- function(l, n) seq(from = -l / 2, to = l / 2, length.out = n)
ptoff_projs_plus_1 <- function(projs, ls, l, cd, cc) projs + 1
csz_equi <- function(nclu, tpts, ae) {
  cs <- vector(mode = "integer", length = nclu)
  for (i in 1:tpts) {
    cs[i %% nclu + 1] = cs[i %% nclu + 1] + 1
  }
  cs
}

# Create parameter combinations to test
targs <- expand.grid(seed = seeds, nd = c(2, 7), ae = allow_empties,
                     ptdist_fn = c("norm", "unif", ptdist_equi),
                     ptoff_fn = c("n-1", "n", ptoff_projs_plus_1),
                     csz_fn = c(clusizes, csz_equi))

# Loop through all parameter combinations
for (i in seq.int(1, nrow(targs))) {

  # Get current parameters
  seed <- targs[i, "seed"]
  ae <- targs[i, "ae"]
  ptdist_fn <- targs[i, "ptdist_fn"][[1]]
  ptoff_fn <- targs[i, "ptoff_fn"][[1]]
  csz_fn <- targs[i, "csz_fn"][[1]]

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
    test_desc <- paste0("clugen optional params: ae=", ae,
                        ", nclu=", nclu, ", tpts=", tpts,
                        ", ptdist_fn='",
                        format(ptdist_fn)[length(format(ptdist_fn))],
                        "', ptoff_fn='",
                        format(ptoff_fn)[length(format(ptoff_fn))],
                        "', czn_fn='",
                        format(csz_fn)[length(format(csz_fn))],
                        "'")

    # Perform tests for current parameter set
    test_that(test_desc, {

      # ...in which case it runs without problem
      expect_warning(r <- clugen(nd, nclu, tpts, direc, astd,
                                 clusep, len_mu, len_std, lat_std,
                                 point_dist_fn = ptoff_fn,
                                 proj_dist_fn = ptdist_fn,
                                 clusizes_fn = csz_fn),
                     regexp = NA)

      # Check dimensions of result variables
      expect_equal(dim(r$points), c(tpts, nd))
      expect_equal(length(r$point_clusters), tpts)
      expect_equal(dim(r$point_projections), c(tpts, nd))
      expect_equal(length(r$cluster_sizes), nclu)
      expect_equal(dim(r$cluster_centers), c(nclu, nd))
      expect_equal(dim(r$cluster_directions), c(nclu, nd))
      expect_equal(length(r$cluster_angles), nclu)
      expect_equal(length(r$cluster_lengths), nclu)

      # Check point cluster indexes
      if (!ae) {
        expect_equal(unique(r$point_clusters), 1:nclu)
      } else {
        expect_true(all(r$point_clusters <= nclu))
      }

      # Check total points
      expect_equal(sum(r$cluster_sizes), tpts)
      # This might not be the case if the specified clusize_fn does not obey
      # the total number of points

      # Check that cluster directions have the correct angles with the main
      # direction
      if (nd > 1) {
        for (i in 1:nclu) {
          expect_equal(angle_btw(direc, r$cluster_directions[i, ]),
                       abs(r$cluster_angles[i]))
        }
      }
    })
  }
}
