#' Package Usage Printing Options
#'
#' @param x A \code{package_usage} data.frame
#' @param warn_percent_usage Minimum percent of functions to be used within a dependent package. Default is \code{20\%}
#' @param warn_number_usage Minimum number of functions to be used within a dependent package. Default is \code{3}
#' @param ... Not used
#'
#' @details
#' Package usage must be below both thresholds for the warning to appear. With the defaults, if a package has fewer than
#' 5 functions then only 1 function is required to prevent a warning message to appear.
#'
#' @method print package_usage
#' @export
print.package_usage <- function(x, warn_percent_usage = 0.2, warn_number_usage = 3, ...) {
 summary(
   x,
   warn_percent_usage = warn_percent_usage,
   warn_number_usage = warn_number_usage,
   ...
  )

  x
}

#' Package Usage Printing Options
#'
#' @param x A \code{package_usage} data.frame
#' @param warn_percent_usage Minimum percent of functions to be used within a dependent package. Default is \code{20\%}
#' @param warn_number_usage Minimum number of functions to be used within a dependent package. Default is \code{3}
#' @param ignore_low_usage_packages A vector of packages to ignore the low usage of, usually when already aware of the
#' low usage, but the dependent package is necessary for the project.
#' @param ... Not used
#'
#' @details
#' Package usage must be below both thresholds for the warning to appear. With the defaults, if a package has fewer than
#' 5 functions then only 1 function is required to prevent a warning message to appear.
#'
#' @method print multi_package_usage
#' @export
print.multi_package_usage <- function(x, warn_percent_usage = 0.2, warn_number_usage = 3,
                                ignore_low_usage_packages = character(), ...) {
  summary(
    x,
    warn_percent_usage = warn_percent_usage,
    warn_number_usage = warn_number_usage,
    ignore_low_usage_packages = ignore_low_usage_packages,
    ...
  )

  x
}
