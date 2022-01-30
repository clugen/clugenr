# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

test_that("points_on_line() works properly", {
  for (seed in c(0, 123, 9999, 9876543)) {
    set.seed(seed)
    for (length in c(0, 10)) {
      for (tps in c(1, 10, 500, 10000)) {

        ctr <- c(1.0, 2.0, 3.0)
        dir <- c(1.0, 0.0, 0.0)
        nd <- length(ctr)

        # Create some random distances from center
        dist2ctr <- length * runif(tpts) - length / 2

        # Check that the points_on_line function runs without warnings
        expect_warning(pts <- points_on_line(ctr, dir, dist2ctr), regexp = NA)

        # Check that the dimensions agree
        expect_equal(dim(pts), c(tpts, nd))

        # Check that distance of points to the line is approximately zero
        for (pt in asplit(pts, 1)) {
          # Get distance from current point to line
          ptv <- as.vector(pt)
          d <- norm(as.matrix((ptv - ctr) - c((ptv - ctr) %*% dir) * dir))
          # Check that it is approximately zero
          expect_equal(d, 0)
        }
      }
    }
  }
})
