testthat::test_that("readRFiles fails when file doesn't exist", {
  testthat::expect_error(
    readRFiles("nonexistent_file")
  )
})

testthat::test_that("readRFiles fails when directory doesn't exist", {
  testthat::expect_error(
    readRFiles("nonexistent_file", "nonexistent_directory")
  )
})

testthat::test_that("readRFiles fails when file doesn't exist in directory", {
  testthat::expect_error(
    readRFiles("nonexistent_file", "r_files")
  )
})

testthat::test_that("readRFiles works when specifying full file path", {
  chunks <- readRFiles("r_files/test.R")

  testthat::expect_gt(length(chunks), 0)
  testthat::expect_type(chunks, "character")
})

testthat::test_that("readRFiles works when specifying file name and directory", {
  chunks <- readRFiles("test.R", "r_files")

  testthat::expect_gt(length(chunks), 0)
  testthat::expect_output(print(chunks), "test1")
  testthat::expect_type(chunks, "character")
})

testthat::test_that("readDirectoryFiles fails when directory doesn't exist", {
  testthat::expect_error(
    readDirectoryRFiles("nonexistent_directory")
  )
})

testthat::test_that("readDirectoryFiles returns empty vector when directory has no R files", {
  testthat::expect_warning(
    readDirectoryRFiles("package"),
    "No R files found in"
  )

  chunks <- suppressWarnings(readDirectoryRFiles("package"))
  testthat::expect_length(chunks, 0)
  testthat::expect_type(chunks, "character")
})

testthat::test_that("readDirectoryFiles returns vector when directory has R files", {
  chunks <- readDirectoryRFiles("r_files")

  testthat::expect_gt(length(chunks), 0)
  testthat::expect_output(print(chunks), "test1")
  testthat::expect_type(chunks, "character")
})

testthat::test_that("readPackageFiles fails when package doesn't exist", {
  testthat::expect_error(
    readPackageRFiles("nonexistent_package")
  )
})

testthat::test_that("readPackageFiles passes when package doesn't exist", {
  chunks <- readPackageRFiles("package")

  testthat::expect_gt(length(chunks), 0)
  testthat::expect_output(print(chunks), "test1")
  testthat::expect_type(chunks, "character")
})
