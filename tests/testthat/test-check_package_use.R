testthat::test_that("getPackageFunctions fails for non-existent package", {
  testthat::expect_error(
    getPackageFunctions("nonexistent_package"),
    "there is no package called"
  )
})

testthat::test_that("getPackageFunctions returns character vector of functions in package", {
  functions <- getPackageFunctions("utils")

  testthat::expect_gt(length(functions), 0)
  testthat::expect_type(functions, "character")
})

testthat::test_that("getInternalPackageFunctions fails for non-existent package", {
  testthat::expect_error(
    getInternalPackageFunctions("nonexistent_package"),
    "there is no package called"
  )
})

testthat::test_that("getPackageFunctions returns character vector of functions in package", {
  functions <- getInternalPackageFunctions("utils")

  testthat::expect_gt(length(functions), 0)
  testthat::expect_type(functions, "character")
})
