# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

test_that("fix_num_points works as expected", {

  # No change
  clusts <- c(10, 100, 42, 0, 12)
  expect_warning(clusts_fixed <- fix_num_points(clusts, sum(clusts)),
                 regexp = NA)
  expect_equal(clusts, clusts_fixed)

  # Fix due to too many points
  clusts <- c(55, 12)
  num_pts <- sum(clusts) - 14
  expect_warning(clusts_fixed <- fix_num_points(clusts, num_pts), regexp = NA)
  expect_false(isTRUE(all.equal(clusts, clusts_fixed)))
  expect_equal(sum(clusts_fixed), num_pts)

  # Fix due to too few points
  clusts <- c(0, 1, 0, 0)
  num_pts <- 15
  expect_warning(clusts_fixed <- fix_num_points(clusts, num_pts), regexp = NA)
  expect_false(isTRUE(all.equal(clusts, clusts_fixed)))
  expect_equal(sum(clusts_fixed), num_pts)

  # 1D - No change
  clusts <- 10
  expect_warning(clusts_fixed <- fix_num_points(clusts, sum(clusts)),
                 regexp = NA)
  expect_equal(clusts, clusts_fixed)

  # 1D - Fix due to too many points
  clusts <- 241
  num_pts <- sum(clusts) - 20
  expect_warning(clusts_fixed <- fix_num_points(clusts, num_pts), regexp = NA)
  expect_false(isTRUE(all.equal(clusts, clusts_fixed)))
  expect_equal(sum(clusts_fixed), num_pts)

  # 1D - Fix due to too few points
  clusts <- 0
  num_pts <- 8
  expect_warning(clusts_fixed <- fix_num_points(clusts, num_pts), regexp = NA)
  expect_false(isTRUE(all.equal(clusts, clusts_fixed)))
  expect_equal(sum(clusts_fixed), num_pts)

})
