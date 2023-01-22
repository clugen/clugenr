# Copyright (c) 2020-2023 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

test_that("fix_empty works as expected", {

  #  No empty clusters
  clusts <- c(11, 21, 10)
  expect_warning(clusts_fixed <- fix_empty(clusts, allow_empty = FALSE),
                 regexp = NA)
  expect_equal(clusts, clusts_fixed)

  # Empty clusters, no fix
  clusts <- c(0, 11, 21, 10, 0, 0)
  expect_warning(clusts_fixed <- fix_empty(clusts, allow_empty = TRUE),
                 regexp = NA)
  expect_equal(clusts, clusts_fixed)

  # Empty clusters, fix
  clusts <- c(5, 0, 21, 10, 0, 0, 101)
  expect_warning(clusts_fixed <- fix_empty(clusts), regexp = NA)
  expect_equal(sum(clusts), sum(clusts_fixed))
  expect_false(isTRUE(all.equal(clusts, clusts_fixed)))
  expect_false(any(clusts_fixed == 0))

  # Empty clusters, fix, several equal maximums
  clusts <- c(101, 5, 0, 21, 101, 10, 0, 0, 101, 100, 99, 0, 0, 0, 100)
  expect_warning(clusts_fixed <- fix_empty(clusts), regexp = NA)
  expect_equal(sum(clusts), sum(clusts_fixed))
  expect_false(isTRUE(all.equal(clusts, clusts_fixed)))
  expect_false(any(clusts_fixed == 0))

  # Empty clusters, no fix (flag)
  clusts <- c(0, 10)
  expect_warning(clusts_fixed <- fix_empty(clusts, allow_empty = TRUE),
                 regexp = NA)
  expect_equal(clusts, clusts_fixed)

  # Empty clusters, no fix (not enough points)
  clusts <- c(0, 1, 1, 0, 0, 2, 0, 0)
  expect_warning(clusts_fixed <- fix_empty(clusts), regexp = NA)
  expect_equal(clusts, clusts_fixed)

  # Works with 1D
  clusts <- 100
  expect_warning(clusts_fixed <- fix_empty(clusts), regexp = NA)
  expect_equal(clusts, clusts_fixed)

})
