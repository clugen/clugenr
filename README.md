# clugenr

## Summary

_clugenr_ is an [R] package for generating multidimensional clusters. Each
cluster is supported by a line segment, the position, orientation and length of
which guide where the respective points are placed.

## How to install

Install the development version from GitHub with the following command (requires
the [devtools] package):

```R
devtools::install_github("clugen/clugenr")
```

<!--
A stable version of the package is available on [CRAN] and can be installed with
the following instruction:

```R
install.packages("clugenr")
```
-->

## Documentation

All methods and functions are fully documented and can be queried using the
built-in help system. After installation, to access the man pages, invoke the
_clugenr_ help page as follows:

```R
help("clugenr")
```

## Examples

2D example:

```R
library(clugenr)
r <- clugen(2, 5, 1000, c(2,3), 0.6, c(4,6), 3, 0.1, 0.5, cluster_offset =  c(1,3), point_dist_fn = "n")
plot(r$points[,1], r$points[,2], pch=19, col=factor(r$point_clusters))
```

3D example:

```R
library(clugenr)
library(rgl) # For plots
r <- clugen(3, 5, 1000, c(2,3,4), 0.5, c(15,13,14), 7, 1, 2, cluster_offset =  c(1,3,3), point_dist_fn = "n")
plot3d(r$points[,1], r$points[,2], r$points[,3], pch=19, col=factor(r$point_clusters))
```


## Development

Test the package:

```r
devtools::test()
```

Check the package:

```r
devtools::check()
```

Build the docs:

```r
devtools::document()
```

Install locally (e.g. after clone):

```r
devtools::install()
```

## License

[MIT License](LICENSE)

[R]: https://www.r-project.org/
[devtools]: https://cran.r-project.org/package=devtools
[CRAN]: https://cran.r-project.org/
