#' Package Usage Printing Options
#'
#' @param x A \code{function_usage} data.frame
#' @param warn_percent_usage Minimum percent of functions to be used within a dependent package. Default is \code{20\%}
#' @param warn_number_usage Minimum number of functions to be used within a dependent package. Default is \code{3}
#' @param ... Not used
#'
#' @details
#' Package usage must be below both thresholds for the warning to appear. With the defaults, if a package has fewer than
#' 5 functions then only 1 function is required to prevent a warning message to appear.
#'
#' @method print function_usage
#' @export
print.function_usage <- function(x, warn_percent_usage = 0.2, warn_number_usage = 3, ...) {
 summary(
   x,
   warn_percent_usage = warn_percent_usage,
   warn_number_usage = warn_number_usage,
   ...
  )
}

#' Package Usage Printing Options
#'
#' @param x A \code{function_usage} data.frame
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
print.package_usage <- function(x, warn_percent_usage = 0.2, warn_number_usage = 3,
                                include_used_functions = TRUE, ...) {
  summary(
    x,
    warn_percent_usage = warn_percent_usage,
    warn_number_usage = warn_number_usage,
    include_used_functions = include_used_functions,
    ...
  )
}
