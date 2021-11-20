#' Check Function Usage in Code
#'
#' @description
#' Counting the frequency that a function is called within selected code.
#'
#' @param function_name Name of the function to check against
#' @param code A character vector of code chunks to check for function use
#' @param package_name Name of the package that \code{function_name} is contained in
#'
#' @details If \code{package_name} is left as \code{NULL}, then there is a chance the checks will find times
#' where the function is used, but is explicitly called from another package.
#'
#' @export
checkFunctionUse <- function(function_name, code, package_name = NULL) {
  if (length(code) == 0) {
    warning("No code to check function usage")
    return(0)
  }

  function_regex <- createFunctionCheckRegEx(function_name, package_name)

  code_split <- strsplit(code, function_regex, perl = TRUE)
  occurrences <- lengths(code_split) - 1

  quote_occurrences <- checkQuotedFunction(code, function_name)

  sum(occurrences - quote_occurrences)
}

createFunctionCheckRegEx <- function(function_name, package_name = NULL, internal = FALSE) {
  function_name_regex <- regexEscape(function_name)

  function_is_word <- grepl("^\\w.*\\w$", function_name)
  if (function_is_word) {
    function_full_regex <- paste0("\\b", function_name_regex, "\\b")
  } else {
    function_full_regex <- function_name_regex
  }

  function_full_regex <- addRegexAssignmentCheck(function_full_regex)

  if (!is.null(package_name)) {
    package_name_regex <- regexEscape(package_name)

    if (internal) {
      operator <- ":::"
    } else {
      operator <- "::"
    }
    function_full_regex <- paste0(
      "[^:]", function_full_regex, "|",
      "\\b", package_name_regex, operator, function_name_regex, "\\b"
    )
  }

  function_full_regex
}

regexEscape <- function(x) {
  gsub(paste0("(", paste0("\\", REGEX_SPECIAL_CHARACTERS, collapse = "|"), ")"), "\\\\\\1", x)
}

REGEX_SPECIAL_CHARACTERS <- c("[", "]", "{", "}", "(", ")", "\\", "^", "$", ".", "|", "?", "*", "+", "%")

addRegexAssignmentCheck <- function(x) {
  paste0(
    addPreviousFunctionAssignmentRegex(x),
    "(?:",
    x,
    FUNCTION_ARGUMENT_REGEX,
    FUNCTION_ASSIGNMENT_REGEX,
    ")"
  )
}

addPreviousFunctionAssignmentRegex <- function(x) {
  paste0("(", x, "\\s*<-.*(*SKIP)(*FAIL)|)")
}

FUNCTION_ARGUMENT_REGEX <- "(?!\\s*=)"
FUNCTION_ASSIGNMENT_REGEX <- "(?!\\s*<-)"

checkQuotedFunction <- function(code, function_name) {
  quoted_substrings <- extractQuotedSubstrings(code)

  function_name_regex <- regexEscape(function_name)

  function_is_word <- grepl("^\\w.*\\w$", function_name)
  if (function_is_word) {
    function_full_regex <- paste0("\\b", function_name_regex, "\\b")
  } else {
    function_full_regex <- function_name_regex
  }

  function_mentions <- lapply(quoted_substrings, grep, pattern = function_full_regex, value = TRUE)
  function_mentions <- lapply(function_mentions, setdiff, y = function_name)
  function_mentions <- lapply(function_mentions, extractSubstrings, regex = function_full_regex)
  lengths(function_mentions)
}

extractSubstrings <- function(code, regex) {
  quote_matches <- gregexpr(regex, code, perl = TRUE)
  substrings <- regmatches(code, quote_matches)
  unlist(substrings)
}

extractQuotedSubstrings <- function(code) {
  quote_matches <- gregexpr(QUOTED_SUBSTRING_REGEX, code, perl = TRUE)
  substrings <- regmatches(code, quote_matches)
  lapply(substrings, gsub, pattern = "^(\"|')|(\"|')$", replacement = "")
}

QUOTED_SUBSTRING_REGEX <- "([\"'])(?:\\\\.|(?!\\1).)*+\\1"
