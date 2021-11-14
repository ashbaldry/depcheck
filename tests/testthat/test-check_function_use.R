testthat::test_that("createFunctionCheckRegEx returns regex expression", {
  package_regex <- createFunctionCheckRegEx("function")

  testthat::expect_type(package_regex, "character")
  testthat::expect_length(package_regex, 1)
  testthat::expect_output(print(package_regex), "function")
})

testthat::test_that("createFunctionCheckRegEx returns regex expression with package", {
  package_regex <- createFunctionCheckRegEx("function", "package")

  testthat::expect_type(package_regex, "character")
  testthat::expect_length(package_regex, 1)
  testthat::expect_output(print(package_regex), "package::function")
})

testthat::test_that("createFunctionCheckRegEx returns regex expression with package as internal function", {
  package_regex <- createFunctionCheckRegEx("function", "package", internal = TRUE)

  testthat::expect_type(package_regex, "character")
  testthat::expect_length(package_regex, 1)
  testthat::expect_output(print(package_regex), "package:::function")
})

testthat::test_that("checkFunctionUse returns 0 when no code given", {
  testthat::expect_warning(
    checkFunctionUse("function", character(0)),
    "No code to check function usage"
  )

  function_use <- suppressWarnings(checkFunctionUse("function", character(0)))
  testthat::expect_type(function_use, "double")
  testthat::expect_identical(function_use, 0)
})

testthat::test_that("checkFunctionUse returns 0 when function not used", {
  function_use <- checkFunctionUse("function_not_used", "test <- function(x) {print('Test function')}")
  testthat::expect_type(function_use, "double")
  testthat::expect_identical(function_use, 0)
})

testthat::test_that("checkFunctionUse returns 1 when function used", {
  function_use <- checkFunctionUse("print", "test <- function(x) {print('Test function')}")
  testthat::expect_type(function_use, "double")
  testthat::expect_identical(function_use, 1)
})

testthat::test_that("checkFunctionUse returns 2 when function used in 2 functions", {
  function_use <- checkFunctionUse(
    "print",
    c("test <- function(x) {print('Test function')}",
      "test2 <- function(x) {print('Test function 2')}")
  )

  testthat::expect_type(function_use, "double")
  testthat::expect_identical(function_use, 2)
})

testthat::test_that("checkFunctionUse returns 2 when function used twice in 1 function", {
  function_use <- checkFunctionUse(
    "print",
    "test <- function(x) {print('Test function'); print('Test function 2')}",
  )

  testthat::expect_type(function_use, "double")
  testthat::expect_identical(function_use, 2)
})
