#' Check Package Use in Code
#'
#' @description
#' This will check what functions contained within a specified package are used in code.
#'
#' @param package_names,package_name Name(s) of the package to check against
#' @param code A character vector of code chunks to check for package use
#' @param verbose Logical, should informative messages be printed during the dependency evaluation?
#'
#' @return
#' \code{checkPackageUsage} will return a data.frame of class \code{package_usage}. When printed it will show
#' a summary of the package usage.
#'
#' \code{checkPackagesUsage} will return a list of class \code{multi_package_usage}. When printed it will show
#' a summary of all packages mentioned, and flag any rarely used.
#'
#' @rdname checkPackageUsage
#' @export
checkPackagesUsage <- function(package_names, code, verbose = TRUE) {
  packages_usage <- lapply(package_names, function(package_name) {
    if (verbose) printPackageName(package_name, package_names)
    checkPackageUsage(package_name, code)
  })
  if (verbose) cat("\r")

  structure(
    packages_usage,
    names = package_names,
    class = c("multi_package_usage", "list")
  )
}

#' @rdname checkPackageUsage
#' @export
checkPackageUsage <- function(package_name, code) {
  functions <- getPackageFunctions(package_name)

  function_usage <- vapply(
    functions,
    checkFunctionUse,
    code = code,
    package_name = package_name,
    FUN.VALUE = numeric(1),
    USE.NAMES = FALSE
  )

  structure(
    data.frame(
      package_name = package_name,
      function_name = functions,
      function_usage = function_usage
    ),
    class = c("package_usage", "data.frame")
  )
}

#' Get Package Functions
#'
#' @description
#' In order to check if packages are included, need to extract all functions from the package.
#'
#' @param package_name Name of the package to find function names for
#'
#' @rdname package_functions
getPackageFunctions <- function(package_name) {
  if (!suppressPackageStartupMessages(requireNamespace(package_name, quietly = TRUE))) {
    stop("Unable to load {", package_name, "}, the package must be installed to check usage.")
  }

  getNamespaceExports(package_name)
}

#' @rdname package_functions
getInternalPackageFunctions <- function(package_name) {
  if (!suppressPackageStartupMessages(requireNamespace(package_name, quietly = TRUE))) {
    stop("Unable to load {", package_name, "}, the package must be installed to check usage.")
  }

  objects <- ls(getNamespace(package_name))
  setdiff(objects, getPackageFunctions(package_name))
}
