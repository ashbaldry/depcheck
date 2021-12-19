#' Function Usage Summary
#'
#' @param object A \code{package_usage} data.frame
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
#' package_use <- checkPackageUsage()
#' dependency_use <- checkPackageDependencyUse()
#' summary(dependency_use)
#' }
#'
#' @method summary package_usage
#' @export
summary.package_usage <- function(object, warn_percent_usage = 0.2, warn_number_usage = 3, ...) {
  package_name <- object$package_name[1]
  n_dependencies <- countDependentPackages(package_name, include_self = FALSE)
  function_usage <- summarisePackageUsage(
    object,
    warn_percent_usage = warn_percent_usage,
    warn_number_usage = warn_number_usage
  )

  package_summary <- list(
    name = package_name,
    n_dependencies = n_dependencies,
    perc_functions_used = function_usage$perc_functions_used,
    functions_used = function_usage$functions_used
  )

  cat(
    "Package: '", package_name, "'\n",
    "Number of Dependencies: ", n_dependencies, "\n",
    "Number of Functions Used: ", function_usage$n_functions_used, " / ", function_usage$n_functions,
    " (", round(function_usage$perc_functions_used * 100), "%)\n",
    if (function_usage$n_functions_used > 0) {
      paste0("Functions Used: ", paste(function_usage$functions_used, collapse = ", "), "\n")
    },
    sep = ""
  )

  if (isTRUE(function_usage$warn_flag)) {
    if (function_usage$n_functions_used) {
      plural <- ""
    } else {
      plural <- "s"
    }

    message(
      "Function usage for '", package_name, "' is below the specified thresholds. ",
      "Consider copying used function", plural, " to reduce dependencies"
    )
  }

  invisible(package_summary)
}

#' Package Usage Summary
#'
#' @param object A \code{package_usage} data.frame
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
#' @method summary multi_package_usage
#' @export
summary.multi_package_usage <- function(object, warn_percent_usage = 0.2, warn_number_usage = 3,
                                        ignore_low_usage_packages = character(), ...) {
  packages_summary <- lapply(
    object,
    summarisePackageUsage,
    warn_percent_usage = warn_percent_usage,
    warn_number_usage = warn_number_usage
  )

  packages <- names(object)
  high_used_packages <- packages[vapply(packages_summary, function(x) !x$warn_flag, logical(1))]
  high_used_dependencies <- findDependentPackages(high_used_packages)
  low_used_packages <- packages[vapply(packages_summary, function(x) x$warn_flag, logical(1))]
  low_used_packages <- setdiff(low_used_packages, c(high_used_dependencies, ignore_low_usage_packages))

  n_dependencies <- countDependentPackages(packages)

  cat(
    "Number of Declared Packages: ", length(object), "\n",
    "Total Number of Dependencies: ", n_dependencies, "\n",
    "Declared Packages: ", paste0(packages, collapse = ", "), "\n",
    sep = ""
  )

  if (length(low_used_packages) > 0) {
    if (length(low_used_packages) == 1) {
      is_verb <- "is"
      low_packages_pasted <- low_used_packages
    } else {
      is_verb <- "are"
      low_packages_pasted <- sub("(.*), ", "\\1 and ", paste0("'", low_used_packages, "'", collapse = ", "))
    }

    message(
      "Function usage for ", low_packages_pasted, " ", is_verb, " below the specified thresholds. ",
      "Print individual package summaries to check if packages can be removed"
    )
  }

  invisible(packages_summary)
}

summarisePackageUsage <- function(package_use, warn_percent_usage = 0.2, warn_number_usage = 3) {
  package_name <- package_use$package_name[1]
  n_functions <- nrow(package_use)
  n_functions_used <- sum(package_use$function_usage > 0)
  perc_functions_used <- n_functions_used / n_functions

  warn_flag <- perc_functions_used <= warn_percent_usage &&
    n_functions_used < warn_number_usage &&
    !package_name %in% BASE_PACKAGES

  list(
    n_functions = n_functions,
    n_functions_used = n_functions_used,
    perc_functions_used = perc_functions_used,
    functions_used = package_use[package_use$function_usage > 0, "function_name"],
    warn_flag = warn_flag
  )
}

countDependentPackages <- function(packages, include_self = TRUE) {
  package_dependencies <- findDependentPackages(packages, include_self)

  if (is.null(package_dependencies)) {
    "NA (offline)"
  } else {
    length(package_dependencies)
  }
}

findDependentPackages <- function(packages, include_self = TRUE) {
  package_dependencies <- tryCatch(
    tools::package_dependencies(packages, recursive = TRUE),
    warning = function(w) NULL
  )

  if (is.null(package_dependencies)) {
    package_dependencies
  } else {
    package_dependencies <- unlist(package_dependencies)
    if (include_self) package_dependencies <- unique(c(packages, package_dependencies))
    setdiff(package_dependencies, BASE_PACKAGES)
  }
}

BASE_PACKAGES <- rownames(installed.packages(priority = "base"))
