printDependencyList <- function(packages) {
  cat("The following packages have been found:", paste(packages, collapse = ", "), "\n")
}

printCheckStart <- function() {
  cat("Checking dependency usage...\n")
}

printCheckEnd <- function() {
  cat("Dependency usage checks complete!", rep(" ", 20), "\n")
}

printPackageName <- function(package, packages) {
  cat("\rChecking ", package, " (", match(package, packages), " / ", length(packages), ")", rep(" ", 40), sep = "")
}
