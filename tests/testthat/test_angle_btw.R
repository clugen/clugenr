# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

test_that("angle_btw works as expected", {

  # Commonly used function for determining the angle between two vectors
  common_angle_btw <- function(v1, v2) {
    acos((v1 %*% v2) / (norm(v1, "2") * norm(v2, "2")))[1]
  }

  # 2D
  u <- c(1.5, 0)
  v <- c(0.1, -0.4)
  expect_equal(angle_btw(u, v), common_angle_btw(u, v))

  # 3D
  u <- c(-1.5, 10, 0)
  v <- c(0.99, 4.4, -1.1)
  expect_equal(angle_btw(u, v), common_angle_btw(u, v))

  # 8D
  u <- c(1.5, 0, 0, 0, 0, 0, 0, -0.5)
  v <- c(7.5, -0.4, 0, 0, 0, -16.4, 0.1, -0.01)
  expect_equal(angle_btw(u, v), common_angle_btw(u, v))
})
