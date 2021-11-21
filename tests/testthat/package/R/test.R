test1 <- function() {
  print("This is a test chunk of code")
}

test2 <- function() {
  testthat::expect_true(TRUE)
}

test3 <- function() {
  testthat::expect_false(FALSE)
}
