# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

points_on_line <- function(center, direction, dist_center) {
  rep(center, each = length(dist_center)) + dist_center %*% matrix(direction, nrow = 1)
}
