testthat::test_that("Dependency check works for package", {
  package_dependency_use <- depcheck::checkPackageDependencyUse("package", verbose = FALSE)
  testthat::expect_s3_class(package_dependency_use, "multi_package_usage")
})

testthat::test_that("Dependency check finds correct function use", {
  package_dependency_use <- depcheck::checkPackageDependencyUse("package", verbose = FALSE)

  testthat::expect_equal(sum(package_dependency_use$formatR$function_usage), 0)

  digest_use <- package_dependency_use$digest
  digest_use <- digest_use[digest_use$function_usage > 0, ]
  testthat::expect_equal(digest_use$function_name, "digest")

  testthat::expect_equal(sum(package_dependency_use$testthat$function_usage), 3)
  testthat_use <- package_dependency_use$testthat
  testthat_use <- testthat_use[testthat_use$function_usage > 0, ]
  testthat::expect_equal(sort(testthat_use$function_name), c("expect_equal", "expect_false", "expect_true"))
})

testthat::test_that("Dependency check shows low use package", {
  package_dependency_use <- depcheck::checkPackageDependencyUse("package", verbose = FALSE)
  testthat::expect_message(print(package_dependency_use), "formatR")
})

testthat::test_that("Dependency check does not show low use of free dependency package", {
  package_dependency_use <- depcheck::checkPackageDependencyUse("package", verbose = FALSE)
  testthat::expect_message(print(package_dependency_use), "^((?!digest).)*$", perl = TRUE)
})

testthat::test_that("Dependency check works for project", {
  project_dependency_use <- depcheck::checkProjectDependencyUse("r_files", verbose = FALSE)
  testthat::expect_s3_class(project_dependency_use, "multi_package_usage")
})
