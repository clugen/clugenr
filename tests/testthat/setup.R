# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

seeds <- c(0, 123)
num_dims <- c(1, 2, 3, 4, 30)
num_points <- c(1, 10, 500, 10000)
num_clusters <- c(1, 2, 5, 10, 100)
lat_stds <- c(0.0, 5.0, 500)
llengths_mus <- c(0, 10)
llengths_sigmas <- c(0, 15)
angles_stds <- c(0, pi / 256, pi / 4, pi / 2, pi, 2 * pi)
allow_empties <- c(TRUE, FALSE)
