# Copyright (c) 2020-2023 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

# ################################## #
# Test clugen() mandatory parameters #
# ################################## #

# Create parameter combinations to test
targs <- expand.grid(seed = seeds, nd = num_dims,
                     nclu = num_clusters,
                     tpts = num_points,
                     astd = angles_stds,
                     len = llengths_mus,
                     len_std = llengths_sigmas,
                     lat_std = lat_stds)

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
  tsargs <- expand.grid(direc = append(asplit(get_vecs(ndirs, nd), 1),
                                       list(matrix(rnorm(nclu * nd),
                                                   ncol = nd))),
                        clusep = asplit(get_clu_seps(nd), 1))

  # Loop through line directions and line centers
  for (j in seq.int(1, nrow(tsargs))) {

    # Get current line direction + line center
    direc <- tsargs[j, "direc"][[1]]
    clusep <- tsargs[j, "clusep"][[1]]

    # Determine test name for current parameter set
    test_desc <- paste0("clugen mandatory params: ",
                        "seed=", seed, ", nd=", nd, ", nclu=", nclu,
                        ", tpts=", tpts, ", astd=", astd, ", len=", len,
                        ", len_std=", len_std, ", lat_std=", lat_std,
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
        expect_equal(length(r$clusters), tpts)
        expect_equal(dim(r$projections), c(tpts, nd))
        expect_equal(length(r$sizes), nclu)
        expect_equal(dim(r$centers), c(nclu, nd))
        expect_equal(dim(r$directions), c(nclu, nd))
        expect_equal(length(r$angles), nclu)
        expect_equal(length(r$lengths), nclu)

        # Check point cluster indexes
        expect_equal(unique(as.numeric(r$clusters)), 1:nclu)

        # Check total points
        expect_equal(sum(r$sizes), tpts)

        # Check that cluster directions have the correct angles with the main
        # direction
        if (nd > 1) {
          # In case direction is just a vector, repeat it num_cluster times
          # into a matrix...
          if (is.vector(direc)
              || (is.array(direc) && length(dim(direc)) == 1)) {
            direc <- matrix(direc,
                            nrow = nclu,
                            ncol = length(direc),
                            byrow = TRUE)
          }
          # ...so we can check each cluster direction separately
          for (i in 1:nclu) {
            expect_equal(angle_btw(direc[i, ], r$directions[i, ]),
                         abs(r$angles[i]))
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
if (testthat:::on_cran() || is_test_mode("cran")) {
  # Light tests, for CRAN
  targs <- expand.grid(seed = seeds, nd = 2, ae = TRUE,
                       prjdist_fn = list("norm", "unif"),
                       ptdist_fn = list("n-1", "n"),
                       csz_fn = list(clusizes),
                       cctr_fn = list(clucenters),
                       llen_fn = list(llengths),
                       lang_fn = list(angle_deltas))
} else {
  # Other testing modes will be more thorough
  targs <- expand.grid(seed = seeds, nd = c(2, 7), ae = allow_empties,
                       prjdist_fn = list("norm", "unif", prjdist_equi),
                       ptdist_fn = list("n-1", "n", ptdist_projs_plus_1),
                       csz_fn = list(clusizes, csz_equi),
                       cctr_fn = list(clucenters, cctr_on_a_line),
                       llen_fn = list(llengths, llen_unif_10_20),
                       lang_fn = list(angle_deltas, lang_same))
}

# Loop through all parameter combinations
for (i in seq.int(1, nrow(targs))) {

  # Get current parameters
  seed <- targs[i, "seed"]
  nd <- targs[i, "nd"]
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
    test_desc <- paste0("clugen optional params: ",
                        "seed = ", seed, ", nd=", nd, ", nclu=", nclu,
                        ", tpts=", tpts, "ae=", ae,
                        ", ptdist_fn='",
                        format(prjdist_fn)[length(format(prjdist_fn))],
                        "', ptoff_fn='",
                        format(ptdist_fn)[length(format(ptdist_fn))],
                        "', czn_fn='",
                        format(csz_fn)[length(format(csz_fn))],
                        "'",
                        "', cctr_fn='",
                        format(cctr_fn)[length(format(cctr_fn))],
                        "'",
                        "', llen_fn='",
                        format(llen_fn)[length(format(llen_fn))],
                        "'",
                        "', lang_fn='",
                        format(lang_fn)[length(format(lang_fn))],
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
                                 angle_deltas_fn = lang_fn,
                                 seed = seed),
                     regexp = NA)

      # Check dimensions of result variables
      expect_equal(dim(r$points), c(tpts, nd))
      expect_equal(length(r$clusters), tpts)
      expect_equal(dim(r$projections), c(tpts, nd))
      expect_equal(length(r$sizes), nclu)
      expect_equal(dim(r$centers), c(nclu, nd))
      expect_equal(dim(r$directions), c(nclu, nd))
      expect_equal(length(r$angles), nclu)
      expect_equal(length(r$lengths), nclu)

      # Check point cluster indexes
      if (!ae) {
        expect_equal(unique(as.numeric(r$clusters)), 1:nclu)
      } else {
        expect_true(all(as.numeric(r$clusters) <= nclu))
      }

      # Check total points
      expect_equal(sum(r$sizes), tpts)
      # This might not be the case if the specified clusize_fn does not obey
      # the total number of points

      # Check that cluster directions have the correct angles with the main
      # direction
      if (nd > 1) {
        for (i in 1:nclu) {
          expect_equal(angle_btw(direc, r$directions[i, ]),
                       abs(r$angles[i]))
        }
      }
    })
  }
}

# ######################################## #
# Test clugen() optional direct parameters #
# ######################################## #

# Valid arguments
astd <- pi / 333
len_mu <- 6
len_std <- 1.1
lat_std <- 1.6

# Create parameter combinations to test
if (testthat:::on_cran() || is_test_mode("cran")) {
  # Light tests, for CRAN
  targs <- expand.grid(seed = seeds, nd = 2, nclu = 6)
} else {
  # Other testing modes will be more thorough
  targs <- expand.grid(seed = seeds, nd = c(1, 5), nclu = c(1, 6))
}

# Loop through all parameter combinations
for (i in seq.int(1, nrow(targs))) {

  # Get current parameters
  seed <- targs[i, "seed"]
  nd <- targs[i, "nd"]
  nclu <- targs[i, "nclu"]

  # Set seed
  set.seed(seed)

  # Create combination of seed-depending parameters
  direc <- rnorm(nd)
  clusep <- 100 * rnorm(nd)
  csz_direct <- sample(1:100, nclu, replace = TRUE)
  cctr_direct <- matrix(100 * rnorm(nd * nclu), nrow = nclu)
  llen_direct <- runif(nclu, min = 0, max = 100)
  lang_direct <- runif(nclu, min = -pi / 2, max = pi / 2)
  tpts <- sum(csz_direct)

  # Determine test name for current parameter set
  test_desc <- paste0("clugen optional params (direct): ",
                      "seed = ", seed, ", nd=", nd, ", nclu=", nclu,
                      ", direc=[", paste(direc, collapse = ", "),
                      ", clusep=[", paste(clusep, collapse = ", "),
                      ", csz_direct=[", paste(csz_direct, collapse = ", "),
                      ", cctr_direct=[", paste(cctr_direct, collapse = ", "),
                      ", llen_direct=[", paste(llen_direct, collapse = ", "),
                      ", lang_direct=[", paste(lang_direct, collapse = ", "))

  # Perform tests for current parameter set
  test_that(test_desc, {

    # ...in which case it runs without problem
    expect_warning(r <- clugen(nd, nclu, tpts, direc, astd,
                               clusep, len_mu, len_std, lat_std,
                               allow_empty = ae,
                               clusizes_fn = csz_direct,
                               clucenters_fn = cctr_direct,
                               llengths_fn = llen_direct,
                               angle_deltas_fn = lang_direct,
                               seed = seed),
                   regexp = NA)

    # Check dimensions of result variables
    expect_equal(dim(r$points), c(tpts, nd))
    expect_equal(length(r$clusters), tpts)
    expect_equal(dim(r$projections), c(tpts, nd))
    expect_equal(length(r$sizes), nclu)
    expect_equal(dim(r$centers), c(nclu, nd))
    expect_equal(dim(r$directions), c(nclu, nd))
    expect_equal(length(r$angles), nclu)
    expect_equal(length(r$lengths), nclu)

    # Check point cluster indexes
    if (!ae) {
      expect_equal(unique(as.numeric(r$clusters)), 1:nclu)
    } else {
      expect_true(all(as.numeric(r$clusters) <= nclu))
    }

    # Check total points
    expect_equal(sum(r$sizes), tpts)
    # This might not be the case if the specified clusize_fn does not obey
    # the total number of points

    # Check that cluster directions have the correct angles with the main
    # direction
    if (nd > 1) {
      for (i in 1:nclu) {
        expect_equal(angle_btw(direc, r$directions[i, ]),
                     abs(r$angles[i]))
      }
    }
  })
}

# ##################### #
# Reproducibility tests #
# ##################### #

# Valid parameters
nd <- 2
nclu <- 4
tpts <- 300
direc <- c(1, 1)
astd <- pi / 64
clusep <- c(7, 6.5)
len_mu <- 4.1
len_std <- 0.5
lat_std <- 0.2

for (seed in seeds) {

  test_that(paste0("clugen reproducibility: seed=", seed), {

    # Get results for run 1 with current seed
    expect_warning(r1 <- clugen(nd, nclu, tpts, direc, astd, clusep,
                                len_mu, len_std, lat_std,
                                seed = seed),
                   regexp = NA)

    # Get results for run 2 with current seed
    expect_warning(r2 <- clugen(nd, nclu, tpts, direc, astd, clusep,
                                len_mu, len_std, lat_std,
                                seed = seed),
                   regexp = NA)

    # Check that results are exactly the same
    expect_equal(r1, r2)
  })
}

# ######################## #
# Test clugen() exceptions #
# ######################## #

for (seed in seeds) {

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
                               angle_deltas_fn = langles_fn,
                               seed = seed),
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
                               angle_deltas_fn = langles_fn,
                               seed = seed),
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
                             angle_deltas_fn = langles_fn,
                             seed = seed),
                 regexp = "Number of dimensions, `num_dims`, must be > 0",
                 fixed = TRUE)

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
                             angle_deltas_fn = langles_fn,
                             seed = seed),
                 regexp = "Number of clusters, `num_clust`, must be > 0",
                 fixed = TRUE)

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
                             angle_deltas_fn = langles_fn,
                             seed = seed),
                 regexp = "`direction` must have magnitude > 0",
                 fixed = TRUE)

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
                             angle_deltas_fn = langles_fn,
                             seed = seed),
                 regexp = paste0("Length of directions in `direction` must be ",
                                 "equal to `num_dims` (",
                                 length(bad_dir), " != ", nd, ")"),
                 fixed = TRUE)

    # Specific direction for each cluster requires one direction per cluster
    expect_error(r <- clugen(nd, nclu, tpts,
                             # but we're passing one extra direction
                             matrix(rnorm(nd * (nclu + 1)), ncol = nd),
                             astd, clusep,
                             len_mu, len_std, lat_std,
                             allow_empty = ae,
                             cluster_offset = clu_off,
                             proj_dist_fn = prj_dist,
                             point_dist_fn = pt_dist,
                             clusizes_fn = csizes_fn,
                             clucenters_fn = ccenters_fn,
                             llengths_fn = llengths_fn,
                             angle_deltas_fn = langles_fn,
                             seed = seed),
                 regexp = paste0("Number of rows in `direction` must be the ",
                                 "same as the number of clusters (",
                                 nclu + 1, " != ", nclu, ")"),
                 fixed = TRUE)

    # Direction needs to be a 1D array (vector) or 2D array (matrix)
    expect_error(r <- clugen(nd, nclu, tpts,
                             # but we're passing a 3D array
                             array(rnorm(nd * nclu * 2), c(nd, nclu, 2)),
                             astd, clusep, len_mu, len_std, lat_std,
                             allow_empty = ae,
                             cluster_offset = clu_off,
                             proj_dist_fn = prj_dist,
                             point_dist_fn = pt_dist,
                             clusizes_fn = csizes_fn,
                             clucenters_fn = ccenters_fn,
                             llengths_fn = llengths_fn,
                             angle_deltas_fn = langles_fn,
                             seed = seed),
                 regexp = paste0("`direction` must be a vector (1D array) ",
                                 "or a matrix (2D array)"),
                 fixed = TRUE)

    # cluster_sep needs to have nd dims
    bad_clusep <- c(10, 10)
    expect_error(r <- clugen(nd, nclu, tpts, direc, astd,
                             bad_clusep,
                             len_mu, len_std, lat_std,
                             allow_empty = ae,
                             cluster_offset = clu_off,
                             proj_dist_fn = prj_dist,
                             point_dist_fn = pt_dist,
                             clusizes_fn = csizes_fn,
                             clucenters_fn = ccenters_fn,
                             llengths_fn = llengths_fn,
                             angle_deltas_fn = langles_fn,
                             seed = seed),
                 regexp = paste0("Length of `cluster_sep` must be equal to ",
                                 "`num_dims` (",
                                 length(bad_clusep), " != ", nd, ")"),
                 fixed = TRUE)

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
                             angle_deltas_fn = langles_fn,
                             seed = seed),
                 regexp = paste0("Length of `cluster_offset` must be equal to ",
                                 "`num_dims` (",
                                 length(bad_cluoff), " != ", nd, ")"),
                 fixed = TRUE)

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
                             angle_deltas_fn = langles_fn,
                             seed = seed),
                 regexp = paste0("`proj_dist_fn` has to be either \"norm\", ",
                                 "\"unif\" or user-defined function"),
                 fixed = TRUE)

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
                             angle_deltas_fn = langles_fn,
                             seed = seed),
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
                             angle_deltas_fn = langles_fn,
                             seed = seed),
                 regexp = paste0("point_dist_fn has to be either \"n-1\",",
                                 " \"n\" or a user-defined function"),
                 fixed = TRUE)

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
                             angle_deltas_fn = langles_fn,
                             seed = seed),
                 regexp = "argument")

    # Invalid direct clusizes
    expect_error(r <- clugen(nd, nclu, tpts, direc, astd, clusep,
                             len_mu, len_std, lat_std,
                             allow_empty = ae,
                             cluster_offset = clu_off,
                             proj_dist_fn = prj_dist,
                             point_dist_fn = pt_dist,
                             clusizes_fn = sample(1:(tpts * nclu), nclu + 1),
                             clucenters_fn = ccenters_fn,
                             llengths_fn = llengths_fn,
                             angle_deltas_fn = langles_fn,
                             seed = seed),
                 regexp = paste0("`clusizes_fn` has to be either a function or",
                                 " a `num_clusters`-sized vector"),
                 fixed = TRUE)

    # Invalid direct clucenters
    expect_error(r <- clugen(nd, nclu, tpts, direc, astd, clusep,
                             len_mu, len_std, lat_std,
                             allow_empty = ae,
                             cluster_offset = clu_off,
                             proj_dist_fn = prj_dist,
                             point_dist_fn = pt_dist,
                             clusizes_fn = csizes_fn,
                             clucenters_fn = matrix(rnorm(nd * (nclu + 1)),
                                                    nrow = nclu + 1),
                             llengths_fn = llengths_fn,
                             angle_deltas_fn = langles_fn,
                             seed = seed),
                 regexp = paste0("clucenters_fn has to be either a function or",
                                 " a matrix of size `num_clusters` x `num_dims`"),
                 fixed = TRUE)

    # Invalid direct llengths
    expect_error(r <- clugen(nd, nclu, tpts, direc, astd, clusep,
                             len_mu, len_std, lat_std,
                             allow_empty = ae,
                             cluster_offset = clu_off,
                             proj_dist_fn = prj_dist,
                             point_dist_fn = pt_dist,
                             clusizes_fn = csizes_fn,
                             clucenters_fn = ccenters_fn,
                             llengths_fn = runif(nclu + 1),
                             angle_deltas_fn = langles_fn,
                             seed = seed),
                 regexp = paste0("`llengths_fn` has to be either a function or",
                                 " a `num_clusters`-sized vector"),
                 fixed = TRUE)

    # Invalid direct langles
    expect_error(r <- clugen(nd, nclu, tpts, direc, astd, clusep,
                             len_mu, len_std, lat_std,
                             allow_empty = ae,
                             cluster_offset = clu_off,
                             proj_dist_fn = prj_dist,
                             point_dist_fn = pt_dist,
                             clusizes_fn = csizes_fn,
                             clucenters_fn = ccenters_fn,
                             llengths_fn = llengths_fn,
                             angle_deltas_fn = runif(nclu + 1),
                             seed = seed),
                 regexp = paste0("`angle_deltas_fn` has to be either a function",
                                 " or a `num_clusters`-sized vector"),
                 fixed = TRUE)

  })
}
