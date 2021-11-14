#' Check Package Dependencies
#'
#' @export
checkPackageDependencies <- function(path = ".", include_suggests = FALSE) {
  path <- normalizePath(path, mustWork = TRUE)
  checkIsPackage(path)

  description <- read.dcf(file.path(path, "DESCRIPTION"))
  description <- setNames(as.list(description), colnames(description))

  dependency_names <- c("Depends", "Imports")
  if (include_suggests) dependency_names <- append(dependency_names, "Suggests")

  if (!any(dependency_names %in% names(description))) {
    cat("No dependency fields found in DESCRIPTION file\n")
    return(TRUE)
  }

  dependencies <- description[intersect(dependency_names, names(description))]
  dependencies <- unlist(strsplit(unlist(dependencies), ",( |\n)*"), use.names = FALSE)
  dependencies <- unique(sub("[^a-zA-Z0-9\\.].*", "", dependencies))
  dependencies <- setdiff(dependencies, "R")

  lapply(dependencies, checkPackageDependency)

  # TODO: Write results
}

checkIsPackage <- function(path) {
  project_files <- list.files(path)
  if (!"DESCRIPTION" %in% project_files) {
    stop("DESCRIPTION file not accessible in ", path, ". Unable to check dependencies")
  }

  invisible(TRUE)
}
