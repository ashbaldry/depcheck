printDependencyList <- function(packages) {
  cat("The following packages have been found:", paste(packages, collapse = ", "), "\n")
}

printCheckStart <- function() {
  cat("Checking dependency usage...\n")
}

printPackageName <- function(package) {
  cat("Checking ", package, sep = "")
}
