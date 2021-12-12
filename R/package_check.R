#' Check Package Dependencies
#'
#' @description
#' `checkPackageDependencyUse()` checks the \code{DESCRIPTION} file for packages mentioned in the \code{Depends}
#' and \code{Imports} fields.
#'
#' Using these packages, it then checks the scripts in the R directory to check for function use of each package.
#'
#' @param path Path to the package root directory.
#' @param include_suggests Logical, should the "Suggests" field also be checked for package dependencies or just
#' the Depends and Imports?
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
#' dependency_use <- checkPackageDependencyUse()
#' summary(dependency_use)
#' }
#'
#' @seealso \code{\link{checkProjectDependencyUse}}, \code{\link{checkShinyDependencyUse}}
#'
#' @export
checkPackageDependencyUse <- function(path = ".", include_suggests = FALSE, verbose = TRUE) {
  path <- normalizePath(path, mustWork = TRUE)
  checkIsPackage(path)

  description_file <- file.path(path, "DESCRIPTION")
  dependencies <- findPackageDependencies(description_file, include_suggests = include_suggests)

  if (length(dependencies) == 0) {
    cat("No dependency fields found in DESCRIPTION file\n")
    return(TRUE)
  } else if (verbose) {
    printDependencyList(dependencies)
  }

  code <- readPackageRFiles(path)

  if (verbose) printCheckStart()
  dependency_usage <- checkPackagesUsage(dependencies, code, verbose)
  if (verbose) printCheckEnd()
  dependency_usage
}

checkIsPackage <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)

  project_files <- list.files(path)
  if (!"DESCRIPTION" %in% project_files) {
    stop("DESCRIPTION file not accessible in ", path, ". Unable to check dependencies")
  }

  if (!"R" %in% project_files) {
    stop("R directory not accessible in ", path, ". Unable to check dependencies")
  }

  invisible(TRUE)
}

findPackageDependencies <- function(description_file, include_suggests = FALSE) {
  description <- read.dcf(description_file)
  description <- structure(as.list(description), names = colnames(description))

  dependency_names <- c("Depends", "Imports")
  if (include_suggests) dependency_names <- append(dependency_names, "Suggests")

  if (!any(dependency_names %in% names(description))) {
    return(character(0))
  }

  dependencies <- description[intersect(dependency_names, names(description))]
  dependencies <- unlist(strsplit(unlist(dependencies), ",( |\n)*"), use.names = FALSE)
  dependencies <- unique(sub("[^a-zA-Z0-9\\.].*", "", dependencies))
  setdiff(dependencies, "R")
}
