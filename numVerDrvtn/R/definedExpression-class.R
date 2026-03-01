# DefinedExpression S4 virtual class
#
# A virtual class with no slots that acts as a marker/mixin for objects that
# participate in numerically-verified derivations.
#
# Any class (e.g., PartitionedMatrix, dgCMatrix, numeric) can gain the
# DefinedExpression interface by listing it in 'contains':
#
#   setClass("MyClass", contains = c("DefinedExpression", ...))
#
# The `:=`, `. ==`, `.eq`, and `.eq.last` operators dispatch on ANY,
# so no slots or methods on DefinedExpression itself are required for basic
# use.  The class exists primarily to:
#   1. Provide a type that can be tested with `is(x, "DefinedExpression")`.
#   2. Allow future method overrides for specific sub-classes.

#' DefinedExpression virtual S4 class
#'
#' @description
#' A virtual S4 class with no slots.  Inherit from this class to signal that an
#' object supports numerically-verified derivation operators (\code{:=},
#' \code{. ==}, \code{.eq}, \code{.eq.last}).
#'
#' Any R class can participate in derivations without inheriting from
#' \code{DefinedExpression} — the operators work on any object.  The class is
#' provided for:
#' \itemize{
#'   \item Type-testing: \code{is(x, "DefinedExpression")}.
#'   \item Method dispatch: sub-classes can override \code{:=} behaviour.
#' }
#'
#' @section Inheritance:
#' \code{PartitionedMatrix} inherits from \code{DefinedExpression} automatically
#' when the \pkg{PartitionedMatrix} package is loaded.  For \code{Matrix} or
#' plain numeric objects no inheritance is needed — they work with the operators
#' as-is.
#'
#' @export
setClass("DefinedExpression", contains = "VIRTUAL")

#' Test whether an object is a DefinedExpression
#'
#' @param x Any R object.
#' @return Logical scalar.
#' @export
#'
#' @examples
#' is.DefinedExpression("hello")  # FALSE
is.DefinedExpression <- function(x) {
  methods::is(x, "DefinedExpression")
}

# ---- dotSentinel class -------------------------------------------------------
# A dedicated S4 sub-class of "character" for the '.' sentinel object.
# Using a specific class ensures our '==' override fires only for the sentinel
# and leaves normal character == behaviour untouched.

#' dotSentinel S4 class (internal)
#'
#' @description
#' Sub-class of \code{"character"} used as the type of the pre-built \code{.}
#' sentinel.  The \code{==} method dispatched on \code{dotSentinel} calls
#' \code{\link{.eq.last}}, enabling the \code{. == expr} derivation-step syntax.
#'
#' @keywords internal
setClass("dotSentinel", contains = "character")
