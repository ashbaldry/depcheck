#' Check Project Package Dependencies
#'
#' @description
#' This...
#'
#' @param path Path to the directories to search for R scripts. By default looks in the current working directory.
#' @param recursive Logical. Should the R file search recurse into directories?
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
checkProjectDependencyUse <- function(path = ".", recursive = TRUE) {
  path <- normalizePath(path, mustWork = TRUE)
  files <- list.files(path, pattern = "\\.(r|R)$", full.names = TRUE, recursive = recursive)

  code <- readRFiles(files)
  dependencies <- extractPackageCalls(code)

  checkPackagesUse(dependencies, code)
}
