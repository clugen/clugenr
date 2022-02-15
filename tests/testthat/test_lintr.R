# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

# Workaround due to bug in lintr: https://github.com/r-lib/lintr/issues/421
if (requireNamespace("lintr", quietly = TRUE)) {
  library(lintr)

  test_that("Package has no lintr errors", {

    # A duplicate copy of the find_package function from lintr
    find_package <- function(path) {
      path <- normalizePath(path, mustWork = FALSE)

      while (!file.exists(file.path(path, "DESCRIPTION"))) {
        path <- dirname(path)
        if (identical(path, dirname(path))) {
          return(NULL)
        }
      }

      path
    }

    if (!is.null(find_package("."))) {
      expect_lint_free(linters = with_defaults(
        cyclocomp_linter = cyclocomp_linter(complexity_limit = 36)))
    }
  })
}
