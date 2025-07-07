# clugenr <img src="man/figures/logo.png" align="right" />

**clugenr** is an R package for generating multidimensional clusters. Each
cluster is supported by a line segment, the position, orientation and length of
which guide where the respective points are placed. The `clugen()` function is
provided for this purpose, as well as a number of auxiliary functions, used
internally and modularly by `clugen()`. Users can swap these auxiliary functions
by their own customized versions, fine-tuning their cluster generation
strategies, or even use them as the basis for their own generation algorithms.

## How to install

A stable version of the package is available on CRAN and can be installed with
the following instruction:

```R
install.packages("clugenr")
```

Alternatively, install the development version from GitHub with the following
command (requires the [devtools] package):

```R
devtools::install_github("clugen/clugenr")
```

## Quick start

```R
library(clugenr)
x <- clugen(2, 5, 1000, c(1, -0.5), 0.5, c(4, 6), 6, 0.2, 0.5)
plot(x$points, col = x$clusters, xlab = "x", ylab = "y", asp = 1)
```

![Scatter plot of 2D clusters generated with clugenr.](man/figures/example2d.png)

```R
library(clugenr)
library(rgl) # For 3D plots
x <- clugen(3, 5, 2000, c(1, 0.5, -0.7), 0.5, c(15, 10, 20), 15, 3, 10)
plot3d(x$points, col = x$clusters, xlab = "x", ylab = "y", zlab = "z", aspect = T)
```

![Scatter plot of 3D clusters generated with clugenr.](man/figures/example3d.png)

## Further reading

* [Theory: _clugen_ algorithm overview](articles/theory.html)
* Detailed usage examples: [2D](articles/examples2d.html),
  3D ([part I](articles/examples3d1.html), [part II](articles/examples3d2.html)),
  [other dimensions](articles/examplesnd.html)
* [Function reference](reference/index.html)
* [Developing this package](articles/dev.html)

[devtools]: https://cran.r-project.org/package=devtools

