testthat::test_that("Package check fails when path is not a package", {
  testthat::expect_error(checkIsPackage("."))
})

testthat::test_that("Package check passes when path is a package", {
  testthat::expect_true(checkIsPackage("package"))
})

testthat::test_that("Package dependencies are found correctly through the DESCRIPTION file", {
  dependencies <- findPackageDependencies(file.path("package", "DESCRIPTION"))

  testthat::expect_true("formatR" %in% dependencies)
  testthat::expect_false("covr" %in% dependencies)
})

testthat::test_that("Suggests dependencies are found correctly when specified", {
  dependencies <- findPackageDependencies(file.path("package", "DESCRIPTION"), include_suggests = TRUE)

  testthat::expect_true("formatR" %in% dependencies)
  testthat::expect_true("covr" %in% dependencies)
})

testthat::test_that("checkPackageDependencyUse works on valid skeleton package", {
  package_check <- checkPackageDependencyUse("package", verbose = FALSE)

  testthat::expect_s3_class(package_check, "multi_package_usage")
})
