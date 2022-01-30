# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

ndirs <- 2
ncts <- 2

for (seed in seeds) {
set.seed(seed)
  for (nd in num_dims) {
    for (len in llengths_mus) {
      for (tpts in num_points[num_points < 1000]) {
        for (direc in asplit(get_unitvecs(ndirs, nd), 1)) {
          for (ctr in asplit(get_vecs(ndirs, nd), 1)) {
            test_desc <- paste0("points_on_line: seed=", seed, ", nd=", nd,
                                ", len=", len, ", tpts=", tpts,
                                ", direc=[", paste(direc, collapse = ", "),
                                "], ctr=[", paste(ctr, collapse = ", "), "]")

            test_that(test_desc, {

              # Create some random distances from center
              dist2ctr <- len * runif(tpts) - len / 2

              # Check that the points_on_line function runs without warnings
              expect_warning(pts <- points_on_line(ctr, direc, dist2ctr),
                             regexp = NA)

              # Check that the dimensions agree
              expect_equal(dim(pts), c(tpts, nd))

              # Check that distance of points to the line is approximately zero
              for (pt in asplit(pts, 1)) {
                # Get distance from current point to line
                ptv <- as.vector(pt)
                dp2l <- (ptv - ctr) - c((ptv - ctr) %*% direc) * direc
                d <- norm(as.matrix(dp2l, type = "2"))
                # Check that it is approximately zero
                expect_equal(d, 0)
              }
            })
          }
        }
      }
    }
  }
}
