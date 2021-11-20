#' Check Package Use in Code
#'
#' @description
#' This will check what functions contained within a specified package are used in code.
#'
#' @param package_name Name of the package to check against
#' @param code A character vector of code chunks to check for package use
#'
#' @return
#' An object of class \code{TODO}
#'
#' @export
checkDependentPackageUsage <- function(package_name, code) {
  functions <- getPackageFunctions(package_name)

  function_usage <- vapply(
    functions,
    checkFunctionUse,
    code = code,
    package_name = package_name,
    FUN.VALUE = numeric(1),
    USE.NAMES = FALSE
  )

  structure(
    data.frame(
      package_name = package_name,
      function_name = functions,
      function_usage = function_usage
    ),
    class = c("package_usage", "data.frame")
  )
}

#' Package Usage Printing Options
#'
#' @param x A \code{package_usage} data.frame
#' @param warn_percent_usage Minimum percent of functions to be used within a dependent package. Default is \code{20\%}
#' @param warn_number_usage Minimum number of functions to be used within a dependent package. Default is \code{3}
#' @param include_used_functions Logical, should the functions that are used be included? Default is \code{TRUE}
#'
#' @details
#' Package usage must be below both thresholds for the warning to appear. With the defaults, if a package has fewer than
#' 5 functions then only 1 function is required to prevent a warning message to appear.
#'
#' @method print package_usage
#' @export
print.package_usage <- function(x, ..., warn_percent_usage = 0.2, warn_number_usage = 3,
                                include_used_functions = TRUE) {
  package_name <- x$package_name[1]
  package_dependencies <- tryCatch(
    tools::package_dependencies(package_name, recursive = TRUE)[[1]],
    warning = function(w) NULL
  )

  if (is.null(package_dependencies)) {
    num_dependencies <- "NA (offline)"
  } else {
    num_dependencies <- length(package_dependencies)
  }


  num_functions <- nrow(x)
  num_functions_used <- sum(x$function_usage > 0)
  perc_functions_used <- num_functions_used / num_functions

  warn_flag <- perc_functions_used <= warn_percent_usage && num_functions_used < warn_number_usage

  functions_used <- x[x$function_usage > 0, "function_name"]

  cat(
    "Package: '", package_name, "'\n",
    "Package Dependencies: ", num_dependencies, "\n",
    "Package Usage: ", num_functions_used, " / ", num_functions, " (", round(perc_functions_used * 100), "%)\n",
    sep = ""
  )

  if (include_used_functions) {
    cat("Functions Used:", paste(functions_used, collapse = ", "), "\n")
  }

  if (warn_flag) {
    message(
      "Function usage for '", package_name, "' is below the specified thresholds. Consider copying used function(s) ",
      "to reduce dependencies"
    )
  }
}

#' Get Package Functions
#'
#' @description
#' In order to check if packages are included, need to extract all functions from the package.
#'
#' @param package_name Name of the package to find function names for
#'
#' @rdname package_functions
getPackageFunctions <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    stop("Unable to load {", package_name, "}, the package must be installed to check usage.")
  }

  getNamespaceExports(package_name)
}

#' @rdname package_functions
getInternalPackageFunctions <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    stop("Unable to load {", package_name, "}, the package must be installed to check usage.")
  }

  objects <- ls(getNamespace(package_name))
  setdiff(objects, getPackageFunctions(package_name))
}
