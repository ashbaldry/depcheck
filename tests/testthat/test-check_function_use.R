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

# Basic Scenarios
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

# More Secnarios
testthat::test_that("checkFunctionUse returns 0 when function is another function argument", {
  function_use <- checkFunctionUse(
    "print",
    "test <- function(x) {test(print = 'Test function')}",
  )

  testthat::expect_identical(function_use, 0)
})

testthat::test_that("checkFunctionUse returns 1 when function is called after being separately used as an argument", {
  function_use <- checkFunctionUse(
    "print",
    "test <- function(x) {test(print = 'Test function'); print('Test output');}",
  )

  testthat::expect_identical(function_use, 1)
})

testthat::test_that("checkFunctionUse returns 1 when function is called before being separately used as an argument", {
  function_use <- checkFunctionUse(
    "print",
    "test <- function(x) {print('Test output'); test(print = 'Test function');}",
  )

  testthat::expect_identical(function_use, 1)
})

testthat::test_that("checkFunctionUse returns 0 when function is assigned new value", {
  function_use <- checkFunctionUse(
    "print",
    "print <- function(x) {print('Test function'); print('Test function 2')}",
  )

  testthat::expect_identical(function_use, 0)
})

testthat::test_that("checkFunctionUse returns 0 on function is part of a substring", {
  function_use <- checkFunctionUse(
    "print",
    "test <- function(x) {cat('I will print the statement')}",
  )

  testthat::expect_identical(function_use, 0)
})

testthat::test_that("checkFunctionUse returns 1 on function is a string", {
  function_use <- checkFunctionUse(
    "print",
    "test <- function(x) {getFromNamespace('print')}",
  )

  testthat::expect_identical(function_use, 1)
})
