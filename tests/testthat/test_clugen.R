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
        expect_equal(unique(as.numeric(r$point_clusters)), 1:nclu)

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
astd <- pi / 256
len_mu <- 9
len_std <- 1.2
lat_std <- 2

# Alternative functions used for testing
prjdist_equi <- function(l, n) seq(from = -l / 2, to = l / 2, length.out = n)
ptdist_projs_plus_1 <- function(projs, ls, l, cd, cc) projs + 1
csz_equi <- function(nclu, tpts, ae) {
  cs <- vector(mode = "integer", length = nclu)
  for (i in 1:tpts) {
    cs[i %% nclu + 1] <- cs[i %% nclu + 1] + 1
  }
  cs
}
cctr_on_a_line <- function(nclu, cs, co) {
  1:nclu * matrix(0, nrow = nclu, ncol = length(cs))
}
llen_unif_10_20 <- function(nclu, l, lsd) stats::runif(nclu, min = 10, max = 20)
lang_same <- function(nclu, astd) vector(mode = "double", length = nclu)

# Create parameter combinations to test
targs <- expand.grid(seed = seeds, nd = c(2, 7), ae = allow_empties,
                     prjdist_fn = c("norm", "unif", prjdist_equi),
                     ptdist_fn = c("n-1", "n", ptdist_projs_plus_1),
                     csz_fn = c(clusizes, csz_equi),
                     cctr_fn = c(clucenters, cctr_on_a_line),
                     llen_fn = c(llengths, llen_unif_10_20),
                     lang_fn = c(angle_deltas, lang_same))

# Loop through all parameter combinations
for (i in seq.int(1, nrow(targs))) {

  # Get current parameters
  seed <- targs[i, "seed"]
  ae <- targs[i, "ae"]
  prjdist_fn <- targs[i, "prjdist_fn"][[1]]
  ptdist_fn <- targs[i, "ptdist_fn"][[1]]
  csz_fn <- targs[i, "csz_fn"][[1]]
  cctr_fn <- targs[i, "cctr_fn"][[1]]
  llen_fn <- targs[i, "llen_fn"][[1]]
  lang_fn <- targs[i, "lang_fn"][[1]]

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
                        format(prjdist_fn)[length(format(prjdist_fn))],
                        "', ptoff_fn='",
                        format(ptdist_fn)[length(format(ptdist_fn))],
                        "', czn_fn='",
                        format(csz_fn)[length(format(csz_fn))],
                        "'")

    # Perform tests for current parameter set
    test_that(test_desc, {

      # ...in which case it runs without problem
      expect_warning(r <- clugen(nd, nclu, tpts, direc, astd,
                                 clusep, len_mu, len_std, lat_std,
                                 allow_empty = ae,
                                 proj_dist_fn = prjdist_fn,
                                 point_dist_fn = ptdist_fn,
                                 clusizes_fn = csz_fn,
                                 clucenters_fn = cctr_fn,
                                 llengths_fn = llen_fn,
                                 angle_deltas_fn = lang_fn),
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
        expect_equal(unique(as.numeric(r$point_clusters)), 1:nclu)
      } else {
        expect_true(all(as.numeric(r$point_clusters) <= nclu))
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

# ######################## #
# Test clugen() exceptions #
# ######################## #

for (seed in seeds) {

  # Set seed
  set.seed(seed)

  # Valid parameters
  nd <- 3
  nclu <- 5
  tpts <- 1000
  direc <- c(1, 0, 0)
  astd <- pi / 64
  clusep <- c(10, 10, 5)
  len_mu <- 5
  len_std <- 0.5
  lat_std <- 0.3
  ae <- TRUE
  clu_off <- c(-1.5, 0, 2)
  prj_dist <- "unif"
  pt_dist <- "n-1"
  csizes_fn <- clusizes
  ccenters_fn <- clucenters
  llengths_fn <- llengths
  langles_fn <- angle_deltas

  test_that(paste0("clugen exceptions: seed=", seed), {

    # Test passes with valid arguments
    expect_warning(r <- clugen(nd, nclu, tpts, direc, astd, clusep,
                               len_mu, len_std, lat_std,
                               allow_empty = ae,
                               cluster_offset = clu_off,
                               proj_dist_fn = prj_dist,
                               point_dist_fn = pt_dist,
                               clusizes_fn = csizes_fn,
                               clucenters_fn = ccenters_fn,
                               llengths_fn = llengths_fn,
                               angle_deltas_fn = langles_fn),
                   regexp = NA)

    # Test passes with zero points since allow_empty is set to true
    expect_warning(r <- clugen(nd, nclu, 0, direc, astd, clusep,
                               len_mu, len_std, lat_std,
                               allow_empty = ae,
                               cluster_offset = clu_off,
                               proj_dist_fn = prj_dist,
                               point_dist_fn = pt_dist,
                               clusizes_fn = csizes_fn,
                               clucenters_fn = ccenters_fn,
                               llengths_fn = llengths_fn,
                               angle_deltas_fn = langles_fn),
                   regexp = NA)

    # Invalid number of dimensions
    expect_error(r <- clugen(0, nclu, tpts, direc, astd, clusep,
                             len_mu, len_std, lat_std,
                             allow_empty = ae,
                             cluster_offset = clu_off,
                             proj_dist_fn = prj_dist,
                             point_dist_fn = pt_dist,
                             clusizes_fn = csizes_fn,
                             clucenters_fn = ccenters_fn,
                             llengths_fn = llengths_fn,
                             angle_deltas_fn = langles_fn),
                 fixed = TRUE,
                 "Number of dimensions, `num_dims`, must be > 0")

    # Invalid number of clusters
    expect_error(r <- clugen(nd, 0, tpts, direc, astd, clusep,
                             len_mu, len_std, lat_std,
                             allow_empty = ae,
                             cluster_offset = clu_off,
                             proj_dist_fn = prj_dist,
                             point_dist_fn = pt_dist,
                             clusizes_fn = csizes_fn,
                             clucenters_fn = ccenters_fn,
                             llengths_fn = llengths_fn,
                             angle_deltas_fn = langles_fn),
                 fixed = TRUE,
                 "Number of clusters, `num_clust`, must be > 0")

    # Direction needs to have magnitude > 0
    expect_error(r <- clugen(nd, nclu, tpts, c(0, 0, 0), astd, clusep,
                             len_mu, len_std, lat_std,
                             allow_empty = ae,
                             cluster_offset = clu_off,
                             proj_dist_fn = prj_dist,
                             point_dist_fn = pt_dist,
                             clusizes_fn = csizes_fn,
                             clucenters_fn = ccenters_fn,
                             llengths_fn = llengths_fn,
                             angle_deltas_fn = langles_fn),
                 fixed = TRUE,
                 "`direction` must have magnitude > 0")

    # Direction needs to have nd dims
    bad_dir <- c(1, 1)
    expect_error(r <- clugen(nd, nclu, tpts, bad_dir, astd, clusep,
                             len_mu, len_std, lat_std,
                             allow_empty = ae,
                             cluster_offset = clu_off,
                             proj_dist_fn = prj_dist,
                             point_dist_fn = pt_dist,
                             clusizes_fn = csizes_fn,
                             clucenters_fn = ccenters_fn,
                             llengths_fn = llengths_fn,
                             angle_deltas_fn = langles_fn),
                 fixed = TRUE,
                 paste0("Length of `direction` must be equal to `num_dims` (",
                        length(bad_dir), " != ", nd, ")"))

    # cluster_offset needs to have nd dims
    bad_cluoff <- c(0, 1)
    expect_error(r <- clugen(nd, nclu, tpts, direc, astd, clusep,
                             len_mu, len_std, lat_std,
                             allow_empty = ae,
                             cluster_offset = bad_cluoff,
                             proj_dist_fn = prj_dist,
                             point_dist_fn = pt_dist,
                             clusizes_fn = csizes_fn,
                             clucenters_fn = ccenters_fn,
                             llengths_fn = llengths_fn,
                             angle_deltas_fn = langles_fn),
                 fixed = TRUE,
                 paste0("Length of `cluster_offset` must be equal to ",
                        "`num_dims` (", length(bad_cluoff), " != ", nd, ")"))

    # Unknown proj_dist_fn given as string
    expect_error(r <- clugen(nd, nclu, tpts, direc, astd, clusep,
                             len_mu, len_std, lat_std,
                             allow_empty = ae,
                             cluster_offset = clu_off,
                             proj_dist_fn = "invalid",
                             point_dist_fn = pt_dist,
                             clusizes_fn = csizes_fn,
                             clucenters_fn = ccenters_fn,
                             llengths_fn = llengths_fn,
                             angle_deltas_fn = langles_fn),
                 fixed = TRUE,
                 paste0("`proj_dist_fn` has to be either \"norm\", \"unif\" or",
                        " user-defined function"))

    # Invalid proj_dist_fn given as function
    expect_error(r <- clugen(nd, nclu, tpts, direc, astd, clusep,
                             len_mu, len_std, lat_std,
                             allow_empty = ae,
                             cluster_offset = clu_off,
                             proj_dist_fn = csizes_fn,
                             point_dist_fn = pt_dist,
                             clusizes_fn = csizes_fn,
                             clucenters_fn = ccenters_fn,
                             llengths_fn = llengths_fn,
                             angle_deltas_fn = langles_fn),
                 "argument")

    # Unknown point_dist_fn given as string
    expect_error(r <- clugen(nd, nclu, tpts, direc, astd, clusep,
                             len_mu, len_std, lat_std,
                             allow_empty = ae,
                             cluster_offset = clu_off,
                             proj_dist_fn = prj_dist,
                             point_dist_fn = "invalid",
                             clusizes_fn = csizes_fn,
                             clucenters_fn = ccenters_fn,
                             llengths_fn = llengths_fn,
                             angle_deltas_fn = langles_fn),
                 fixed = TRUE,
                 paste0("point_dist_fn has to be either \"n-1\", \"n\" or ",
                        "a user-defined function"))

    # Invalid point_dist_fn given as function
    expect_error(r <- clugen(nd, nclu, tpts, direc, astd, clusep,
                             len_mu, len_std, lat_std,
                             allow_empty = ae,
                             cluster_offset = clu_off,
                             proj_dist_fn = prj_dist,
                             point_dist_fn = llengths_fn,
                             clusizes_fn = csizes_fn,
                             clucenters_fn = ccenters_fn,
                             llengths_fn = llengths_fn,
                             angle_deltas_fn = langles_fn),
                 "argument")

  })
}
