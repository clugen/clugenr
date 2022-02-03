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
