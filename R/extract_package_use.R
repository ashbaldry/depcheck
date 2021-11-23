#' Extract Package Calls
#'
#' @description
#' The ability to search for package loading (via \code{library()} or \code{requireNamespace}), or by direct
#' referencing through \code{"pkg::function"}
#'
#' @param code A character vector of code chunks to check for package calls
extractPackageCalls <- function(code) {
  explicit_calls <- extractExplicitPackageCalls(code)
  import_calls <- extractImportPackageCalls(code)

  unique(c(explicit_calls, import_calls))
}

extractExplicitPackageCalls <- function(code) {
  package_mentions <- gregexpr(EXPLICIT_PACKAGE_CALL_REGEX, code, perl = TRUE)
  packages <- regmatches(code, package_mentions)
  unique(unlist(packages))
}

EXPLICIT_PACKAGE_CALL_REGEX <- paste0("\\b(", .standard_regexps()[["valid_package_name"]], ")(?=:{2,3})")

extractImportPackageCalls <- function(code) {
  package_mentions <- gregexpr(PACKAGE_IMPORT_CALL_REGEX, code, perl = TRUE)
  packages <- regmatches(code, package_mentions)
  packages <- unlist(packages)
  unique(sub(PACKAGE_IMPORT_CALL_PREFIX_REGEX, "", packages))
}

IMPORT_FUNCTIONS <- c("library", "require", "requireNamespace", "loadNamespace")
PACKAGE_IMPORT_CALL_PREFIX_REGEX <- paste0("((", paste0(IMPORT_FUNCTIONS, collapse = "|"), ")\\(['\"]{0,1})")
PACKAGE_IMPORT_CALL_REGEX <- paste0(
  PACKAGE_IMPORT_CALL_PREFIX_REGEX,
  .standard_regexps()[["valid_package_name"]],
  "(?![^'\"].+character.only\\s?=\\s?T)"
)
