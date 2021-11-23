#' Check Shiny Application Dependencies
#'
#' @description
#' This...
#'
#' @param path Path to the application root directory
#' @param r_scripts_dir Subdirectories in the shiny application that contain R scripts, write as relative paths to
#' \code{path}. Default is set to \code{R}
#'
#' @examples
#' \dontrun{
#' dependency_use <- checkShinyDependencyUse()
#' summary(dependency_use)
#' }
#'
#' @seealso \code{\link{checkProjectDependencyUse}}, \code{\link{checkPackageDependencyUse}}
#'
#' @export
checkShinyDependencyUse <- function(path = ".", r_scripts_dir = "R") {
  path <- normalizePath(path, mustWork = TRUE)
  checkIsShinyApp(path)

  files <- findShinyRFiles(path, r_scripts_dir)

  code <- readRFiles(files)
  dependencies <- extractPackageCalls(code)

  checkPackagesUse(dependencies, code)
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
