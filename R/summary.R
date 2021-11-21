#' Function Usage Summary
#'
#' @param object A \code{function_usage} data.frame
#' @param warn_percent_usage Minimum percent of functions to be used within a dependent package. Default is \code{20\%}
#' @param warn_number_usage Minimum number of functions to be used within a dependent package. Default is \code{3}
#' @param ... Not used
#'
#' @details
#' Function usage must be below both thresholds for the warning to appear. With the defaults,
#' if a package has fewer than 5 functions then only 1 function is required to prevent a warning message to appear.
#'
#' @examples
#' \dontrun{
#' package_use <- checkPackageUse()
#' dependency_use <- checkPackageDependencies()
#' summary(dependency_use)
#' }
#'
#' @method summary function_usage
#' @export
summary.function_usage <- function(object, warn_percent_usage = 0.2, warn_number_usage = 3, ...) {
  package_name <- object$package_name[1]
  package_dependencies <- tryCatch(
    tools::package_dependencies(package_name, recursive = TRUE)[[1]],
    warning = function(w) NULL
  )

  if (is.null(package_dependencies)) {
    num_dependencies <- "NA (offline)"
  } else {
    num_dependencies <- length(package_dependencies)
  }


  num_functions <- nrow(object)
  num_functions_used <- sum(object$function_usage > 0)
  perc_functions_used <- num_functions_used / num_functions

  warn_flag <- perc_functions_used <= warn_percent_usage && num_functions_used < warn_number_usage

  functions_used <- object[object$function_usage > 0, "function_name"]

  cat(
    "Package: '", package_name, "'\n",
    "Package Dependencies: ", num_dependencies, "\n",
    "Package Usage: ", num_functions_used, " / ", num_functions, " (", round(perc_functions_used * 100), "%)\n",
    "Functions Used: ", paste(functions_used, collapse = ", "), "\n",
    sep = ""
  )

  if (warn_flag) {
    message(
      "Function usage for '", package_name, "' is below the specified thresholds. ",
      "Consider copying used function(s) to reduce dependencies"
    )
  }

  object
}

#' Package Usage Summary
#'
#' @param object A \code{function_usage} data.frame
#' @param warn_percent_usage Minimum percent of functions to be used within a dependent package. Default is \code{20\%}
#' @param warn_number_usage Minimum number of functions to be used within a dependent package. Default is \code{3}
#' @param include_used_functions Logical, should the functions that are used be included? Default is \code{TRUE}
#' @param ... Not used
#'
#' @details
#' Package usage must be below both thresholds for the warning to appear. With the defaults, if a package has fewer than
#' 5 functions then only 1 function is required to prevent a warning message to appear.
#'
#' @method print package_usage
#' @export
summary.package_usage <- function(object, warn_percent_usage = 0.2, warn_number_usage = 3,
                                  include_used_functions = TRUE, ...) {
  lapply(
    object,
    print,
    warn_percent_usage = warn_percent_usage,
    warn_number_usage = warn_number_usage,
    include_used_functions = include_used_functions
  )

  object
}
