# Package-level environment for numVerDrvtn
#
# Replaces the use of <<- for global state in the original definedExpression.R.
# All mutable package state lives here, avoiding pollution of .GlobalEnv.

.nvd_env <- new.env(parent = emptyenv())

# ---- Options ---------------------------------------------------------------

#' Set or query numVerDrvtn options
#'
#' @description
#' Controls package-level behaviour such as whether differences are printed
#' when `.eq()` or `. ==` detects a mismatch.
#'
#' @param print_differences Logical.  When \code{TRUE} (default), `.eq()` and
#'   `. ==` print the output of \code{all.equal()} when expressions differ.
#'
#' @return Invisibly returns the previous value of \code{print_differences}.
#' @export
#'
#' @examples
#' definedExpression.options(print_differences = FALSE)
#' definedExpression.options(print_differences = TRUE)
definedExpression.options <- function(print_differences = TRUE) {
  prev <- .nvd_env$print_differences
  .nvd_env$print_differences <- print_differences
  invisible(prev)
}

# Initialise defaults
.nvd_env$print_differences <- TRUE
.nvd_env$comparison_reference <- NULL
