#' Check Package Dependencies
#'
#' @export
checkPackageDependencies <- function(path = ".", include_suggests = FALSE) {
  path <- normalizePath(path, mustWork = TRUE)
  checkIsPackage(path)

  description_file <- file.path(path, "DESCRIPTION")
  dependencies <- findPackageDependencies(description_file, include_suggests = include_suggests)

  if (length(dependencies) == 0) {
    cat("No dependency fields found in DESCRIPTION file\n")
    return(TRUE)
  }

  code <- readPackageRFiles(path)

  lapply(dependencies, checkDependentPackageUsage, code = code)

  # TODO: Write results
}

checkIsPackage <- function(path) {
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
  description <- setNames(as.list(description), colnames(description))

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
