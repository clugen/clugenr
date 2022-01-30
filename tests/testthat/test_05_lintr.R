# Copyright (c) 2020-2022 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

test_that("Package has no lintr errors", {
  lintr::expect_lint_free(
    linters = lintr::with_defaults(cyclocomp_linter = NULL))
  })
