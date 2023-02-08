# Copyright (c) 2020-2023 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

test_that("Package linting", {
  # Don't do linting on CRAN, coverage reporting or testing on CI + Not Windows
  skip_if(is_test_mode("cran"))
  skip_on_cran()
  skip_on_covr()
  skip_if(testthat:::system_os() != "windows" && testthat:::on_ci())
  pkgpath <- if (testthat:::on_ci()) {
    Sys.getenv("GITHUB_WORKSPACE")
  } else {
    getwd()
  }
  lintr::expect_lint_free(
    path = pkgpath,
    linters = lintr::linters_with_defaults(
      # The clugen() function has a large cyclomatic complexity
      # so let's increase this a bit
      cyclocomp_linter = lintr::cyclocomp_linter(complexity_limit = 42),
      # The official line length for this package is 80, but lets
      # give it some margin; also, Windows complains about the 80
      # limit due to not recognizing some UTF-8 characters.
      line_length_linter = lintr::line_length_linter(88)
    )
  )
})
