# Derivation operators for numVerDrvtn
#
# Provides the := assignment operator, the '. ==' equivalence-checking syntax,
# and helper functions .eq, .eq.last, and .asgn.
#
# Design notes:
#  - comparison_reference is stored in .nvd_env (see package-env.R) rather
#    than in .GlobalEnv via <<-, to avoid side-effects outside the package.
#  - The '.' sentinel is a dotSentinel S4 object (sub-class of character).
#    A setMethod("==", c("dotSentinel", "ANY")) intercepts '. == expr' calls
#    and routes them to .eq.last(), without affecting normal character ==.
#  - ':=' stores drop0'd sparse matrices and any other value as-is, unlike
#    the original definedExpression.R which called singlePartMatrix() —
#    that coupling is removed here.

# ---- Sentinel object '. ' ---------------------------------------------------

#' The dot sentinel for derivation-step comparisons
#'
#' @description
#' A pre-built \code{\link{dotSentinel}} object exported as \code{.}.
#' Use \code{. == expr} after a \code{:=} or \code{.asgn()} call to compare
#' \code{expr} against the previous \code{comparison_reference}.
#'
#' @details
#' The \code{. == expr} syntax works because \code{.} is an S4
#' \code{dotSentinel} object, and a method \code{setMethod("==",
#' c("dotSentinel", "ANY"))} routes the call to \code{.eq.last()}.
#'
#' @export
#' @examples
#' \dontrun{
#' A := some_matrix_expression
#' . == equivalent_expression   # prints differences if any, returns TRUE/FALSE
#' }
. <- methods::new("dotSentinel", ".")

# ---- setMethod for '. ==' ---------------------------------------------------

setMethod("==", signature(e1 = "dotSentinel", e2 = "ANY"), function(e1, e2) {
  .eq.last(e2)
})

# ---- := operator ------------------------------------------------------------

#' Assignment with printing and reference-setting for derivation steps
#'
#' @description
#' Assigns \code{expression_assigned} to \code{new_variable} in the calling
#' frame, prints the result, and stores it as \code{comparison_reference} for
#' subsequent \code{. == expr} or \code{.eq.last()} calls.
#'
#' Sparse \link[Matrix:sparseMatrix]{Matrix} objects are cleaned up with
#' \code{drop0(tol = 1e-12)} before assignment.
#'
#' @param new_variable Unquoted name to assign to in the calling environment.
#' @param expression_assigned Expression to evaluate, assign, print, and
#'   record as the current comparison reference.
#' @param ... Currently unused; reserved for future options.
#'
#' @return Invisibly returns \code{expression_assigned} (after any sparse
#'   clean-up).
#' @export
#'
#' @examples
#' \dontrun{
#' A := Matrix::Matrix(c(1,0,0,2), 2, 2, sparse = TRUE)
#' . == Matrix::Matrix(c(1,0,0,2), 2, 2, sparse = TRUE)   # TRUE
#' }
`:=` <- function(new_variable, expression_assigned, ...) {
  # Clean up sparse near-zeroes
  if (methods::is(expression_assigned, "sparseMatrix")) {
    expression_assigned <- Matrix::drop0(expression_assigned, tol = 1e-12)
  }
  var_name <- deparse(substitute(new_variable))
  assign(var_name, expression_assigned, envir = parent.frame())
  print(expression_assigned)
  .nvd_env$comparison_reference <- expression_assigned
  invisible(expression_assigned)
}

# ---- .asgn ------------------------------------------------------------------

#' Assign a variable and set the comparison reference (function form of :=)
#'
#' @description
#' A function alternative to \code{:=} for contexts where an operator cannot
#' be used (e.g., inside a list).  Assigns \code{expression_assigned} to
#' \code{new_variable} in the calling frame and sets it as the
#' \code{comparison_reference}.
#'
#' @param new_variable Unquoted name to assign in the calling environment.
#' @param expression_assigned Expression to evaluate and assign.
#'
#' @return Invisibly returns \code{expression_assigned}.
#' @export
.asgn <- function(new_variable, expression_assigned) {
  if (methods::is(expression_assigned, "sparseMatrix")) {
    expression_assigned <- Matrix::drop0(expression_assigned, tol = 1e-12)
  }
  .nvd_env$comparison_reference <- expression_assigned
  assign(deparse(substitute(new_variable)), expression_assigned, envir = parent.frame())
  invisible(expression_assigned)
}

# ---- .eq: compare two expressions -------------------------------------------

#' Compare two expressions numerically
#'
#' @description
#' Calls \code{\link[base]{all.equal}(left_expression, right_expression, tol)}
#' and optionally prints differences when the result is not \code{TRUE}.
#'
#' @param left_expression,right_expression Expressions to compare.
#' @param tol Numeric tolerance passed to \code{all.equal()}.  Default
#'   \code{1e-9}.
#' @param print_diff Logical override for whether to print differences.  If
#'   \code{NULL} (default), uses the package option set by
#'   \code{\link{definedExpression.options}}.
#'
#' @return Invisibly returns \code{TRUE} if expressions are equal within
#'   tolerance, \code{FALSE} otherwise.
#' @export
#'
#' @examples
#' .eq(1 + 1e-12, 1, tol = 1e-9)  # TRUE
#' .eq(1 + 0.1,   1, tol = 1e-9)  # FALSE (and prints difference)
.eq <- function(left_expression, right_expression, tol = 1e-9, print_diff = NULL) {
  result <- all.equal(left_expression, right_expression, tolerance = tol)
  print_diffs <- if (!is.null(print_diff)) print_diff else .nvd_env$print_differences
  if (!isTRUE(result) && print_diffs) {
    cat(paste(result, collapse = "\n"), "\n")
  }
  invisible(isTRUE(result))
}

# ---- .eq.last: compare against comparison_reference -------------------------

#' Compare an expression against the most recent := assignment
#'
#' @description
#' Calls \code{\link{.eq}(comparison_reference, right_expression, tol)} where
#' \code{comparison_reference} is whatever was last assigned via \code{:=} or
#' \code{.asgn()}.
#'
#' This is also the function invoked by the \code{. == expr} syntax.
#'
#' @param right_expression Expression to compare against the stored reference.
#' @param tol Numeric tolerance.  Default \code{1e-9}.
#'
#' @return Invisibly returns \code{TRUE} or \code{FALSE}.
#' @export
#'
#' @examples
#' \dontrun{
#' A := some_matrix
#' .eq.last(equivalent_rearrangement)
#' }
.eq.last <- function(right_expression, tol = 1e-9) {
  stopifnot(!is.null(right_expression))
  ref <- .nvd_env$comparison_reference
  if (is.null(ref)) {
    stop(
      ".eq.last(): comparison_reference is NULL. ",
      "Use := or .asgn() to set a reference before calling .eq.last()."
    )
  }
  .eq(ref, right_expression, tol)
}
