#' Read R Files
#'
#' @description
#' Reading R files for package dependency checks.
#'
#' \code{readPackageFiles} assumes the selected path is a package, and will check the \code{R} subdirectory for files
#' to read. \code{readDirectoryFiles} is a more generic version of \code{readPackageFiles}
#'
#' @param path Location of the package or directory to import R files from
#'
#' @details
#' The default directory for \code{readFiles} is \code{NULL}, to allow the reading of files from multiple directories.
#'
#' @return
#' A character vector containing unique code chunks in the specified directory/files.
#'
#' @examples
#' \dontrun{
#' readPackageRFiles()
#'
#' # When running outside of a package
#' readDirectoryRFiles("R")
#' }
#'
#' @rdname read_file
#' @export
readPackageRFiles <- function(path = ".") {
  path <- normalizePath(path, mustWork = TRUE)
  checkIsPackage(path)

  readDirectoryRFiles(file.path(path, "R"))
}

#' @rdname read_file
#' @export
readDirectoryRFiles <- function(path = ".") {
  path <- normalizePath(path, mustWork = TRUE)

  files <- list.files(path, pattern = "\\.(r|R)$")
  if (length(files) == 0) {
    warning("No R files found in ", path)
    return(character(0))
  }

  readRFiles(files, path)
}

#' @param files Vector of R file paths to import
#' @rdname read_file
#' @export
readRFiles <- function(files, path = NULL) {
  if (is.null(path)) {
    files <- normalizePath(files, mustWork = TRUE)
  } else {
    files <- normalizePath(file.path(path, files), mustWork = TRUE)
  }

  if (!all(grepl("\\.(R|r)$", files))) {
    stop("Not all files selected are 'R' files. Make sure all files have the '.R' extension")
  }

  code <- lapply(files, formatR::tidy_source, comment = FALSE, blank = FALSE, output = FALSE)
  code <- unlist(lapply(code, function(x) x$text.tidy), use.names = FALSE, recursive = FALSE)

  unique(code)
}
