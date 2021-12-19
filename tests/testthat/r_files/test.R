test1 <- function() {
  print("This is a test chunk of code")
}

test2 <- function() {
  testthat::expect_true(TRUE)
}

test3 <- function() {
  testthat::expect_false(FALSE)
}

test3 <- function() {
  testthat::expect_equal(5, 5)
}

digest_function <- function() {
  digest::digest("fd")
}
