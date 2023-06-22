# Copyright (c) 2020-2023 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

#' Generate multidimensional clusters
#'
#' @description
#' \loadmathjax
#' This is the main function of **clugenr**, and possibly the only function most
#' users will need.
#'
#' @details
#' If a custom function was given in the `clusizes_fn` parameter, it is
#' possible that `num_points` may have a different value than what was
#' specified in the `num_points` parameter.
#'
#' The terms "average" and "dispersion" refer to measures of central
#' tendency and statistical dispersion, respectively. Their exact meaning
#' depends on the optional arguments.
#'
#' @note This function is stochastic. For reproducibility set a PRNG seed with
#' [set.seed].
#'
#' @param num_dims Number of dimensions.
#' @param num_clusters Number of clusters to generate.
#' @param num_points Total number of points to generate.
#' @param direction Average direction of the cluster-supporting lines. Can be
#' a vector of length `num_dims` (same direction for all clusters) or a
#' matrix of size `num_clusters` x `num_dims` (one direction per cluster).
#' @param angle_disp Angle dispersion of cluster-supporting lines (radians).
#' @param cluster_sep Average cluster separation in each dimension (vector of
#' length `num_dims`).
#' @param llength Average length of cluster-supporting lines.
#' @param llength_disp Length dispersion of cluster-supporting lines.
#' @param lateral_disp Cluster lateral dispersion, i.e., dispersion of points
#' from their projection on the cluster-supporting line.
#' @param allow_empty Allow empty clusters? `FALSE` by default.
#' @param cluster_offset Offset to add to all cluster centers (vector of length
#' `num_dims`). By default there will be no offset.
#' @param proj_dist_fn Distribution of point projections along
#' cluster-supporting lines, with three possible values:
#' - `"norm"` (default): Distribute point projections along lines using a normal
#'   distribution (\mjeqn{\mu=}{μ=} _line_center_,
#'   \mjeqn{\sigma=}{σ=} `llength/6` ).
#' - `"unif"`: Distribute points uniformly along the line.
#' - User-defined function, which accepts two parameters, line length (`double`)
#'   and number of points (`integer`), and returns a vector containing the
#'   distance of each point projection to the center of the line. For example,
#'   the `"norm"` option roughly corresponds to
#'   `function(l, n) stats::rnorm(n, sd = l / 6)`.
#' @param point_dist_fn Controls how the final points are created from their
#' projections on the cluster-supporting lines, with three possible values:
#' - `"n-1"` (default): Final points are placed on a hyperplane orthogonal to
#'   the cluster-supporting line, centered at each point's projection, using the
#'   normal distribution (\mjeqn{\mu=0}{μ=0},
#'   \mjeqn{\sigma=}{σ=} `lateral_disp` ). This is done by the [clupoints_n_1]
#'   function.
#' - `"n"`: Final points are placed around their projection on the
#'   cluster-supporting line using the normal distribution (\mjeqn{\mu=0}{μ=0},
#'   \mjeqn{\sigma=}{σ=} `lateral_disp` ). This is done by the [clupoints_n]
#'   function.
#' - User-defined function: The user can specify a custom point placement
#'   strategy by passing a function with the same signature as [clupoints_n_1]
#'   and [clupoints_n].
#' @param clusizes_fn Distribution of cluster sizes. By default, cluster sizes
#' are determined by the [clusizes] function, which uses the normal distribution
#' (\mjeqn{\mu=}{μ=} `num_points`/`num_clusters`, \mjeqn{\sigma=\mu/3}{σ=μ/3}),
#' and assures that the final cluster sizes add up to `num_points`. This
#' parameter allows the user to specify a custom function for this purpose,
#' which must follow [clusizes] signature. Note that custom functions are not
#' required to strictly obey the `num_points` parameter. Alternatively, the user
#' can specify a vector of cluster sizes directly.
#' @param clucenters_fn Distribution of cluster centers. By default, cluster
#' centers are determined by the [clucenters] function, which uses the uniform
#' distribution, and takes into account the `num_clusters` and `cluster_sep`
#' parameters for generating well-distributed cluster centers. This parameter
#' allows the user to specify a custom function for this purpose, which must
#' follow [clucenters] signature. Alternatively, the user can specify a matrix
#' of size `num_clusters` x `num_dims` with the exact cluster centers.
#' @param llengths_fn Distribution of line lengths. By default, the lengths of
#' cluster-supporting lines are determined by the [llengths] function, which
#' uses the folded normal distribution (\mjeqn{\mu=}{μ=} `llength`,
#' \mjeqn{\sigma=}{σ=} `llength_disp` ). This parameter allows the user to
#' specify a custom function for this purpose, which must follow [llengths]
#' signature. Alternatively, the user can specify a vector of line lengths
#' directly.
#' @param angle_deltas_fn Distribution of line angle differences with respect to
#' `direction`. By default, the angles between the main `direction` of each
#' cluster and the final directions of their cluster-supporting lines are
#' determined by the [angle_deltas] function, which uses the wrapped normal
#' distribution (\mjeqn{\mu=0}{μ=0}, \mjeqn{\sigma=}{σ=} `angle_disp` ) with
#' support in the interval \mjeqn{\left[-\pi/2,\pi/2\right]}{[-π/2, π/2]}. This
#' parameter allows the user to specify a custom function for this purpose,
#' which must follow [angle_deltas] signature. Alternatively, the user can
#' specify a vector of angle deltas directly.
#' @param seed An integer used to initialize the PRNG, allowing for reproducible
#' results. If specified, `seed` is simply passed to [set.seed].
#' @return A named list with the following elements:
#' - `points`: A `num_points` x `num_dims` matrix with the generated points for
#'   all clusters.
#' - `clusters`: A `num_points` factor vector indicating which cluster
#'    each point in `points` belongs to.
#' - `projections`: A `num_points` x `num_dims` matrix with the point
#'   projections on the cluster-supporting lines.
#' - `sizes`: A `num_clusters` x 1 vector with the number of points in
#'   each cluster.
#' - `centers`: A `num_clusters` x `num_dims` matrix with the
#'   coordinates of the cluster centers.
#' - `directions`: A `num_clusters` x `num_dims` matrix with the final
#'   direction of each cluster-supporting line.
#' - `angles`: A `num_clusters` x 1 vector with the angles between the
#'   cluster-supporting lines and the main direction.
#' - `lengths`: A `num_clusters` x 1 vector with the lengths of the
#'   cluster-supporting lines.
#'
#' @export
#' @examples
#' # 2D example
#' x <- clugen(2, 5, 1000, c(1, 3), 0.5, c(10, 10), 8, 1.5, 2)
#' graphics::plot(x$points, col = x$clusters, xlab = "x", ylab = "y", asp = 1)
#' # 3D example
#' x <- clugen(3, 5, 1000, c(2, 3, 4), 0.5, c(15, 13, 14), 7, 1, 2)
clugen <- function(num_dims, num_clusters, num_points, direction, angle_disp,
                   cluster_sep, llength, llength_disp, lateral_disp,
                   allow_empty = FALSE, cluster_offset = NA, proj_dist_fn = "norm",
                   point_dist_fn = "n-1", clusizes_fn = clusizes,
                   clucenters_fn = clucenters, llengths_fn = llengths,
                   angle_deltas_fn = angle_deltas, seed = NA) {

  # ############### #
  # Validate inputs #
  # ############### #

  # Check that number of dimensions is > 0
  if (num_dims < 1) {
    stop("Number of dimensions, `num_dims`, must be > 0")
  }

  # Check that number of clusters is > 0
  if (num_clusters < 1) {
    stop("Number of clusters, `num_clust`, must be > 0")
  }

  # Is direction a vector or a matrix?
  if (is.vector(direction) ||
      (is.array(direction) && length(dim(direction)) == 1)) {
    # If a main direction vector was given, transpose it, so we can treat it
    # like a matrix later
    direction <- matrix(direction, ncol = length(direction))
  } else if (is.matrix(direction)) {
    # If a matrix was given (i.e. a main direction is given for each cluster),
    # check if the number of directions is the same as the number of clusters
    dir_size_1 <- dim(direction)[1]
    if (dir_size_1 != num_clusters) {
      stop("Number of rows in `direction` must be the same as the number of ",
           "clusters (", dir_size_1, " != ", num_clusters, ")")
    }
  } else {
    # The `direction` array must be a vector or a matrix, so if we get here
    # it means we have invalid arguments
    stop("`direction` must be a vector (1D array) or a matrix (2D array)")
  }

  # Check that `direction` has num_dims dimensions
  dir_size_2 <- dim(direction)[2]
  if (dir_size_2 != num_dims) {
    stop("Length of directions in `direction` must be equal to `num_dims` (",
         dir_size_2, " != ", num_dims, ")")
  }

  # Check that all directions in `direction` have magnitude > 0
  if (isTRUE(all.equal(min(apply(direction, 1, norm, type = "2")), 0))) {
    stop("`direction` must have magnitude > 0")
  }

  # If allow_empty is false, make sure there are enough points to distribute
  # by the clusters
  if (!allow_empty && num_points < num_clusters) {
    stop("A total of ", num_points, " points is not enough for ",
         num_clusters, " non-empty clusters")
  }

  # Check that cluster_sep has num_dims dimensions
  if (length(cluster_sep) != num_dims) {
    stop("Length of `cluster_sep` must be equal to `num_dims` (",
      length(cluster_sep), " != ", num_dims, ")")
  }

  # If given, cluster_offset must have the correct number of dimensions,
  # if not given then it will be a num_dims x 1 vector of zeros
  if (length(cluster_offset) == 1 && is.na(cluster_offset)) {
    cluster_offset <- vector(mode = "integer", length = num_dims)
  } else if (length(cluster_offset) != num_dims) {
    stop("Length of `cluster_offset` must be equal to `num_dims` (",
      length(cluster_offset), " != ", num_dims, ")")
  }

  # Check that proj_dist_fn specifies a valid way for projecting points along
  # cluster-supporting lines i.e., either "norm" (default), "unif" or a
  # user-defined function
  if (is.function(proj_dist_fn)) {
    # Use user-defined distribution; assume function accepts length of line
    # and number of points, and returns a number of points x 1 vector
    pointproj_fn <- proj_dist_fn
  } else if (is.character(proj_dist_fn) && proj_dist_fn == "unif") {
    # Point projections will be uniformly placed along cluster-supporting lines
    pointproj_fn <- function(l, n) stats::runif(n, min = -l / 2, max = l / 2)
  } else if (is.character(proj_dist_fn) && proj_dist_fn == "norm") {
    # Use normal distribution for placing point projections along
    # cluster-supporting lines, mean equal to line center, standard deviation
    # equal to 1/6 of line length such that the line length contains ≈99.73% of
    # the points
    pointproj_fn <- function(l, n) stats::rnorm(n, sd = (1.0 / 6.0) * l)
  } else {
    stop("`proj_dist_fn` has to be either \"norm\", \"unif\" or ",
         "user-defined function")
  }

  # Check that point_dist_fn specifies a valid way for generating points given
  # their projections along cluster-supporting lines, i.e., either "n-1"
  # (default), "n" or a user-defined function
  if (num_dims == 1) {
    # If 1D was specified, point projections are the points themselves
    pt_from_proj_fn <- function(projs, lat_disp, len, clu_dir, clu_ctr) projs
  } else if (is.function(point_dist_fn)) {
    # Use user-defined distribution; assume function accepts point projections
    # on the line, lateral disp., cluster direction and cluster center, and
    # returns a num_points x num_dims matrix containing the final points
    # for the current cluster
    pt_from_proj_fn <- point_dist_fn
  } else if (is.character(point_dist_fn) && point_dist_fn == "n-1") {
    # Points will be placed on a hyperplane orthogonal to the cluster-supporting
    # line using a normal distribution centered at their intersection
    pt_from_proj_fn <- clupoints_n_1
  } else if (is.character(point_dist_fn) && point_dist_fn == "n") {
    # Points will be placed using a multivariate normal distribution
    # centered at the point projection
    pt_from_proj_fn <- clupoints_n
  } else {
    stop("point_dist_fn has to be either \"n-1\", \"n\" or a user-defined ",
         "function")
  }

  # If seed was given, set it
  if (!is.na(seed)) {
    set.seed(seed)
  }

  # ############################ #
  # Determine cluster properties #
  # ############################ #

  # Normalize main direction(s)
  direction <- matrix(apply(direction, 1, function(x) x / norm(x, "2")),
                      ncol = num_dims,
                      byrow = TRUE)

  # If only one main direction was given, expand it for all clusters
  if (dim(direction)[1] == 1) {
    direction <- matrix(direction,
                        nrow = num_clusters,
                        ncol = length(direction),
                        byrow = TRUE)
  }

  # Determine cluster sizes
  cluster_sizes <- if (is.function(clusizes_fn)) {
    clusizes_fn(num_clusters, num_points, allow_empty)
  } else if (is.vector(clusizes_fn) && length(clusizes_fn) == num_clusters) {
    clusizes_fn
  } else {
    stop("`clusizes_fn` has to be either a function or a ",
         "`num_clusters`-sized vector")
  }

  # Custom clusizes_fn's are not required to obey num_points, so we update
  # it here just in case it's different from what the user specified
  num_points <- sum(cluster_sizes)

  # Determine cluster centers
  cluster_centers <- if (is.function(clucenters_fn)) {
    clucenters_fn(num_clusters, cluster_sep, cluster_offset)
  } else if (is.matrix(clucenters_fn) &&
             all(dim(clucenters_fn) == c(num_clusters, num_dims))) {
    clucenters_fn
  } else {
    stop("clucenters_fn has to be either a function or a matrix of size ",
         "`num_clusters` x `num_dims`")
  }

  # Determine length of lines supporting clusters
  cluster_lengths <- if (is.function(llengths_fn)) {
    llengths_fn(num_clusters, llength, llength_disp)
  } else if (is.vector(llengths_fn) && length(llengths_fn) == num_clusters) {
    llengths_fn
  } else {
    stop("`llengths_fn` has to be either a function or a `num_clusters`-sized",
         " vector")
  }

  # Obtain angles between main direction and cluster-supporting lines
  cluster_angles <- if (is.function(angle_deltas_fn)) {
    angle_deltas_fn(num_clusters, angle_disp)
  } else if (is.vector(angle_deltas_fn) && length(angle_deltas_fn) == num_clusters) {
    angle_deltas_fn
  } else {
    stop("`angle_deltas_fn` has to be either a function or a ",
         "`num_clusters`-sized vector")
  }

  # Determine normalized cluster directions
  cluster_directions <- matrix(mapply(rand_vector_at_angle,
                                      asplit(direction, 1),
                                      cluster_angles),
                               nrow = num_clusters,
                               byrow = TRUE)

  # ################################# #
  # Determine points for each cluster #
  # ################################# #

  # Aux. vector with cumulative sum of number of points in each cluster
  cumsum_points <- c(0, cumsum(cluster_sizes))

  # Pre-allocate data structures for holding cluster info and points

  # Cluster indices of each point
  point_clusters <- vector(mode = "integer", length = num_points)
  # Point projections on cluster-supporting lines
  point_projections <- matrix(data = 0, nrow = num_points, ncol = num_dims)
  # Final points to be generated
  points <- matrix(data = 0, nrow = num_points, ncol = num_dims)

  # Loop through clusters and create points for each one
  for (i in 1:num_clusters) {

    # Skip iteration if this cluster is empty
    if (cluster_sizes[i] == 0) {
      next
    }

    # Start and end indexes for points in current cluster
    idx_start <- cumsum_points[i] + 1
    idx_end <- cumsum_points[i + 1]

    # Update cluster indices of each point
    point_clusters[idx_start:idx_end] <- i

    # Determine distance of point projections from the center of the line
    ptproj_dist_fn_center <- pointproj_fn(cluster_lengths[i], cluster_sizes[i])

    # Determine coordinates of point projections on the line using the
    # parametric line equation (this works since cluster direction is
    # normalized)
    proj  <- points_on_line(cluster_centers[i, ],
                            cluster_directions[i, ],
                            ptproj_dist_fn_center)
    point_projections[idx_start:idx_end, ] <- proj

    # Determine points from their projections on the line
    points[idx_start:idx_end, ] <- pt_from_proj_fn(proj,
                                                   lateral_disp,
                                                   cluster_lengths[i],
                                                   cluster_directions[i, ],
                                                   cluster_centers[i, ])
  }

  # Return result
  list(points = points,
       clusters = as.factor(point_clusters),
       projections = point_projections,
       sizes = cluster_sizes,
       centers = cluster_centers,
       directions = cluster_directions,
       angles = cluster_angles,
       lengths = cluster_lengths)
}

#' Generality of types, by increasing order
#' @keywords internal
tprom <- list("logical" = 1,
              "integer" = 2,
              "double" = 3,
              "complex" = 4,
              "character" = 5,
              "raw" = 6,
              "list" = 7)

#' Returns the actual dimensions of the input
#'
#' Gets the dimensions of the input `a`, returning `c(length(a), 1)` if
#' `dim(a) == NULL`.
#'
#' @keywords internal
gdim <- function(a) {
  d <- dim(a)
  if (is.null(d)) {
    d <- c(length(a), 1)
  }
  d
}

#' Merges the fields (specified in `fields`) of two or more data sets
#'
#' @description
#' Merges the fields (specified in `fields`) of two or more data sets (passed as
#' lists). The fields to be merged need to have the same number of columns. The
#' corresponding merged field will contain the rows of the fields to be merged,
#' and will have a common "supertype".
#'
#' @details
#' The `clusters_field` parameter specifies a field containing integers that
#' identify the cluster to which the respective points belongs to. If
#' `clusters_field` is specified (by default it's specified as `"clusters"`),
#' cluster assignments in individual datasets will be updated in the merged
#' dataset so that clusters are considered separate. This parameter can be set
#' to `NA`, in which case no field will be considered as a special cluster
#' assignments field.
#'
#' This function can be used to merge data sets generated with the [clugen]
#' function, by default merging the `points` and `clusters` fields in those data
#' sets. It also works with arbitrary data by specifying alternative fields in
#' the `fields` parameter. It can be used, for example, to merge third-party
#' data with [clugen]-generated data.
#'
#' @param ... One or more cluster data sets (in the form of lists) whose
#' `fields` are to be merged.
#' @param fields Fields to be merged, which must exist in the data sets given in
#' `...`.
#' @param cluster_field Field containing the integer cluster labels. If
#' specified, cluster assignments in individual datasets will be updated in the
#' merged dataset so that clusters are considered separate.
#' @return A list whose fields consist of the merged fields in the original
#' data sets.
#'
#' @export
#' @examples
#' a <- clugen(2, 5, 100, c(1, 3), 0.5, c(10, 10), 8, 1.5, 2)
#' b <- clugen(2, 3, 250, c(-1, 3), 0.5, c(13, 14), 7, 1, 2)
#' ab <- clumerge(a, b)
clumerge <- function(...,
                     fields = c("points", "clusters"),
                     clusters_field = "clusters") {

  # Number of elements in each array the merged dataset
  numel <- 0

  # Contains information about each field
  fields_info <- list()

  # Merged dataset to output, initially empty
  output <- list()

  # If a clusters field is given, add it
  if (!is.na(clusters_field)) {
    fields <- union(fields, clusters_field)
  }

  # Place variable input into list form
  data <- list(...)

  # Cycle through data items
  for (dt in data) {

    # Number of elements in the current item
    numel_i <- NA

    # Cycle through fields for the current item
    for (field in fields) {

      if (!(field %in% names(dt))) {
        stop(paste0("Data item does not contain required field `", field, "`"))
      }

      # Get the field value
      value <- dt[[field]]

      # Number of elements in field value
      numel_tmp <- gdim(value)[1]

      # Check the number of elements in the field value
      if (is.na(numel_i)) {
        # First field: get number of elements in value (must be the same
        # for the remaining field values)
        numel_i <- numel_tmp
      } else if (numel_tmp != numel_i) {
        # Fields values after the first must have the same number of elements
        stop(paste0("Data item contains fields with different sizes (",
                    numel_tmp, " != ", numel_i, ")"))
      }

      # Get/check info about the field value type
      field_cols <- gdim(value)[2]
      field_type <- typeof(value)
      field_fact <- is.factor(value)

      if (!(field %in% names(fields_info))) {

        # If it's the first time this field appears, just get the info
        fields_info[[field]] <- list(type = field_type,
                                     ncol = field_cols,
                                     fact = field_fact)

        # If it's the clusters field, it needs to have no dimensionality
        if (!is.na(clusters_field) && field == clusters_field && !is.null(dim(value))) {
          stop(paste0("Clusters field `",
                      clusters_field,
                      "` has more than one dimension"))
        }

      } else {
        # If this field already appeared in previous data items...

        # ...check that the number of columns is the same than that of previous
        # data items
        if (field_cols != fields_info[[field]]$ncol) {
          stop(paste0("Dimension mismatch in field `", field, "`"))
        }

        # ...check if previous items were factors, this item must be too (or
        # vice-versa)
        if (field_fact != fields_info[[field]]$fact) {
          stop(paste0("Factor mismatch in field `", field, "`"))
        }

        # Determine most broad type between values
        if (tprom[[field_type]] > fields_info[[field]]$type) {
          fields_info[[field]]$type <- tprom[[field_type]]
        }
      }
    }

    # Update total number of elements
    numel <- numel + numel_i
  }

  # Initialize output dictionary fields with room for all items
  for (field in names(fields_info)) {
    output[[field]] <- vector(mode = fields_info[[field]]$type,
                              length = numel * fields_info[[field]]$ncol)

    if (fields_info[[field]]$ncol > 1) {
      dim(output[[field]]) <- c(numel, fields_info[[field]]$ncol)
    }
  }

  # Copy items from input data to output dictionary, field-wise
  copied <- 0
  last_cluster <- 0

  # Create merged output
  for (dt in data) {

    # How many elements to copy for the current data item?
    tocopy <- gdim(dt[[1]])[1]

    # Cycle through each field and its information
    for (field in names(fields_info)) {

      ncol <- fields_info[[field]]$ncol
      # Copy elements
      if (ncol == 1) {
        output[[field]][(copied + 1):(copied + tocopy)] <-
          if (!is.na(clusters_field) && field == clusters_field) {
            # If this is a clusters field, update the cluster IDs
            old_clusters <- unique(dt[[clusters_field]])
            new_clusters <- (last_cluster + 1):(last_cluster + length(old_clusters))
            last_cluster <- new_clusters[length(new_clusters)]
            new_clusters[match(dt[[field]], old_clusters)]
          } else {
            # Otherwise just copy the elements
            dt[[field]]
          }
      } else {
        output[[field]][(copied + 1):(copied + tocopy), ] <- dt[[field]]
      }
    }

    # Update how many were copied so far
    copied <- copied + tocopy
  }

  # Set factor according to original data
  for (field in names(fields_info)) {
    fi <- fields_info[[field]]
    if (fi$fact) {
      output[[field]] <- factor(output[[field]])
    }
  }

  output
}
