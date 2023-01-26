[![Checks](https://github.com/clugen/clugenr/actions/workflows/check.yml/badge.svg)](https://github.com/clugen/clugenr/actions/workflows/check.yml)
[![codecov](https://codecov.io/gh/clugen/clugenr/branch/main/graph/badge.svg?token=3MWG03OYS5)](https://app.codecov.io/gh/clugen/clugenr)
[![docs](https://img.shields.io/badge/docs-latest-blue.svg)](https://clugen.github.io/clugenr/)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/clugenr)](https://CRAN.R-project.org/package=clugenr)
![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/clugenr)
[![MIT](https://img.shields.io/badge/license-MIT-yellowgreen.svg)](https://tldrlegal.com/license/mit-license)

# clugenr <img src="man/figures/logo.png" align="right" />

## Summary

**clugenr** is an [R] implementation of the *clugen* algorithm generating
multidimensional clusters. Each cluster is supported by a line segment, the
position, orientation and length of which guide where the respective points are
placed.

## How to install

A stable version of the package is available on [CRAN] and can be installed with
the following instruction:

```R
install.packages("clugenr")
```

Alternatively, install the development version from GitHub with the following
command (requires the [devtools] package):

```R
devtools::install_github("clugen/clugenr")
```

## Documentation

All methods and functions are fully documented and can be queried using the
built-in help system. After installation, to access the man pages, invoke the
`clugenr` help page as follows:

```R
help("clugenr")
```

The complete online documentation is available [here](https://clugen.github.io/clugenr/).

## Quick start

```R
library(clugenr)
x <- clugen(2, 5, 1000, c(1, -0.5), 0.5, c(4, 6), 6, 0.2, 0.5)
plot(x$points, col = x$clusters, xlab = "x", ylab = "y", asp = 1)
```

![](man/figures/example2d.png)

```R
library(clugenr)
library(rgl) # For 3D plots
x <- clugen(3, 5, 2000, c(1, 0.5, -0.7), 0.5, c(15, 10, 20), 15, 3, 10)
plot3d(x$points, col = x$clusters, xlab = "x", ylab = "y", zlab = "z", aspect = T)
```

![](man/figures/example3d.png)

## See also

* [pyclugen](https://github.com/clugen/pyclugen/), a Python implementation of
  the *clugen* algorithm.
* [CluGen.jl](https://github.com/clugen/CluGen.jl/), an Julia implementation of
  the *clugen* algorithm.
* [MOCluGen](https://github.com/clugen/MOCluGen/), a MATLAB/Octave implementation
  of the *clugen* algorithm.

## Reference

If you use this software, please cite the following reference:

* Fachada, N. & de Andrade, D. (2023). Generating Multidimensional Clusters With
  Support Lines. <https://doi.org/10.48550/arXiv.2301.10327>.

## License

[MIT License](LICENSE)

[R]: https://www.r-project.org/
[devtools]: https://cran.r-project.org/package=devtools
[CRAN]: https://cran.r-project.org/
