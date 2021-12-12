#' Check Project Package Dependencies
#'
#' @description
#' `checkProjectDependencyUse()` checks R scripts within a project for package calling through the use of
#' \code{library}, \code{require} and \code{requireNamespace}. It also finds packages explicitly used via
#' the \code{pkg::function} notation.
#'
#' Using these packages, it then checks the same R scripts to check for function use of each package.
#'
#' @param path Path to the directories to search for R scripts. By default looks in the current working directory.
#' @param recursive Logical. Should the R file search recurse into directories?
#' @param verbose Logical, should informative messages be printed during the dependency evaluation?
#'
#' @return
#' An object of class \code{multi_package_usage}, a named list of the dependencies used, each containing a
#' \code{data.frame} of all the functions within the package and how often they are used within the project.
#'
#' The result will flag any packages that have been rarely used in the project.
#'
#' @examples
#' \dontrun{
#' dependency_use <- checkProjectDependencyUse()
#' summary(dependency_use)
#' }
#'
#' @seealso \code{\link{checkPackageDependencyUse}}, \code{\link{checkShinyDependencyUse}}
#'
#' @export
checkProjectDependencyUse <- function(path = ".", recursive = TRUE, verbose = TRUE) {
  path <- normalizePath(path, mustWork = TRUE)
  files <- list.files(path, pattern = "\\.(r|R)$", full.names = TRUE, recursive = recursive)

  code <- readRFiles(files)
  dependencies <- extractPackageCalls(code)

  if (length(dependencies) == 0) {
    cat("No dependencies found in project\n")
    return(TRUE)
  } else if (verbose) {
    printDependencyList(dependencies)
  }

  if (verbose) printCheckStart()
  dependency_usage <- checkPackagesUsage(dependencies, code, verbose)
  if (verbose) printCheckEnd()
  dependency_usage
}
