#' Check Shiny Application Dependencies
#'
#' @description
#' `checkShinyDependencyUse()` checks the global.R and app.R (or ui.R and server.R) scripts within a project, and
#' any specified directory within the project,  for package calling through the use of
#' \code{library}, \code{require} and \code{requireNamespace}. It also finds packages explicitly used via
#' the \code{pkg::function} notation.
#'
#' Using these packages, it then checks the same R scripts to check for function use of each package.
#'
#' @param path Path to the application root directory
#' @param r_scripts_dir Subdirectories in the shiny application that contain R scripts, write as relative paths to
#' \code{path}. Default is set to \code{R}
#' @param verbose Logical, should informative messages be printed during the dependency evaluation?
#'
#' @examples
#' \dontrun{
#' dependency_use <- checkShinyDependencyUse()
#' summary(dependency_use)
#' }
#'
#' @return
#' An object of class \code{multi_package_usage}, a named list of the dependencies used, each containing a
#' \code{data.frame} of all the functions within the package and how often they are used within the project.
#'
#' The result will flag any packages that have been rarely used in the project.
#'
#' @seealso \code{\link{checkProjectDependencyUse}}, \code{\link{checkPackageDependencyUse}}
#'
#' @export
checkShinyDependencyUse <- function(path = ".", r_scripts_dir = "R", verbose = TRUE) {
  path <- normalizePath(path, mustWork = TRUE)
  checkIsShinyApp(path)

  files <- findShinyRFiles(path, r_scripts_dir)

  code <- readRFiles(files)
  dependencies <- extractPackageCalls(code)

  if (length(dependencies) == 0) {
    cat("No dependency fields found in shiny application\n")
    return(TRUE)
  } else if (verbose) {
    printDependencyList(dependencies)
  }

  if (verbose) printCheckStart()
  dependency_usage <- checkPackagesUsage(dependencies, code, verbose)
  if (verbose) printCheckEnd()
  dependency_usage
}

checkIsShinyApp <- function(path) {
  project_files <- tolower(list.files(path))

  if (!("app.r" %in% project_files || all(c("ui.r", "server.r") %in% project_files))) {
    stop("Neither ui.R and server.R nor app.R found in ", path, ". Unable to check dependencies")
  }

  invisible(TRUE)
}

findShinyRFiles <- function(path = ".", r_scripts_dir = "R") {
  path <- normalizePath(path, mustWork = TRUE)

  top_files <- list.files(path, "^(app|global|server|ui).r$", full.names = TRUE, ignore.case = TRUE)
  files <- list.files(file.path(path, r_scripts_dir), pattern = "\\.(r|R)$", full.names = TRUE)
  unique(c(files, top_files))
}
