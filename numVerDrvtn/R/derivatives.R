# Derivative verification infrastructure for numVerDrvtn
#
# Provides functions for numerically verifying analytical matrix derivatives.
# Uses numDeriv::jacobian() for numerical Jacobian computation, with a
# caching layer keyed on (function body, parameter value, arguments).
#
# Ported from definedExpression.R by Mark Thallman (USDA-ARS, USMARC).
#
# Key cleanups vs. the original script-based code:
#  - <<- / assign(..., envir = .GlobalEnv) replaced with .nvd_env storage.
#  - digest::digest() used for Jacobian cache keys (unchanged).
#  - TODO: allow numeric (non-Matrix) parameter and result types throughout.

# ---- DerivativeContext constructor ------------------------------------------

#' Create a new derivative context
#'
#' @description
#' Allocates a \code{DerivativeContext} environment that stores the named
#' argument list used by derivative functions (\code{\link{df_dp}},
#' \code{\link{dv_dv}}) and a Jacobian cache to avoid redundant computation.
#'
#' Typically called once per derivation block, then passed to
#' \code{\link{init_derivative_args}()} and subsequently made available via
#' \code{\link{get_context}()}.
#'
#' @param args A named list of arguments (model matrices, etc.) that will be
#'   forwarded to derivative functions via \code{\link{get_args}()}.
#'
#' @return A \code{DerivativeContext} environment with members
#'   \code{derivative_args} (an environment of the supplied named values) and
#'   \code{jacobian_cache} (an empty environment for caching computed
#'   Jacobians).
#' @export
#'
#' @seealso \code{\link{init_derivative_args}}, \code{\link{get_context}}
new_derivative_context <- function(args) {
  ctx <- new.env(parent = emptyenv())
  ctx$derivative_args <- list2env(args, envir = new.env(parent = emptyenv()))
  ctx$jacobian_cache  <- new.env(parent = emptyenv())
  class(ctx) <- "DerivativeContext"
  return(ctx)
}

# ---- init_derivative_args ---------------------------------------------------

#' Initialise a derivative context from named arguments
#'
#' @description
#' Convenience wrapper that constructs a \code{DerivativeContext} from
#' \code{...} and stores it as the \emph{current} context (retrievable via
#' \code{\link{get_context}()}).  All arguments must be named.
#'
#' @param ... Named arguments to include in the derivative context.  These are
#'   the model objects (matrices, scalars, …) that derivative functions will
#'   need access to.
#'
#' @return The newly created \code{DerivativeContext} (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' init_derivative_args(X = X_mat, Z = Z_mat, G = G_mat)
#' ctx <- get_context()
#' }
init_derivative_args <- function(...) {
  args <- list(...)
  if (is.null(names(args)) || any(names(args) == "" | is.na(names(args)))) {
    stop("init_derivative_args(): all arguments must be named.")
  }
  ctx <- new_derivative_context(args)
  .nvd_env$last_deriv_ctx <- ctx
  invisible(ctx)
}

# ---- get_context ------------------------------------------------------------

#' Retrieve the current derivative context
#'
#' @description
#' Returns the \code{DerivativeContext} most recently created by
#' \code{\link{init_derivative_args}()}.  Raises an error if no context has
#' been initialised.
#'
#' @return A \code{DerivativeContext} environment.
#' @export
get_context <- function() {
  ctx <- .nvd_env$last_deriv_ctx
  if (is.null(ctx)) {
    stop("get_context(): no derivative context has been initialised. ",
         "Call init_derivative_args() first.")
  }
  return(ctx)
}

# ---- get_args ---------------------------------------------------------------

#' Restore derivative arguments into the calling environment
#'
#' @description
#' Intended to be called \emph{only} from inside a derivative function as the
#' default value of its \code{args} parameter.  It:
#' \enumerate{
#'   \item Unpacks \code{ctx$derivative_args} into the calling frame.
#'   \item Determines the name of \code{parm_value}'s parameter from the call
#'     stack and attaches it as \code{attr(parm_value, "parm_name")}.
#'   \item Propagates \code{parm_name} so nested derivative functions can
#'     track which parameter they are differentiating with respect to.
#' }
#'
#' @param ctx A \code{DerivativeContext} as returned by
#'   \code{\link{get_context}()}.
#' @param parm_value The parameter value being differentiated.
#'
#' @return Invisibly returns \code{ctx$derivative_args} as a list; called
#'   primarily for its side effects.
#' @export
get_args <- function(ctx, parm_value) {
  list2env(as.list(ctx$derivative_args), envir = parent.frame())

  parm_name <- get_arg_name_from_caller(1)
  if (!is.null(attr(parm_value, "parm_name"))) {
    parm_name <- attr(parm_value, "parm_name")
  } else if (parm_name %in% c("parm_value", "temp")) {
    stop(sprintf(
      'get_args(): attr(parm_value, "parm_name") is NULL and parm_name = %s',
      parm_name
    ))
  } else {
    attr(parm_value, "parm_name") <- parm_name
    assign("parm_value", parm_value, envir = parent.frame())
  }

  if (is.null(ctx$derivative_args[[parm_name]])) {
    ctx$derivative_args[[parm_name]] <- parm_value
  } else {
    assign(parm_name, parm_value, envir = parent.frame())
  }
  invisible(as.list(ctx$derivative_args))
}

# ---- get_arg_name_from_caller -----------------------------------------------

#' Get the name of the argument at a given position from two call levels up
#'
#' @description
#' Internal helper for \code{\link{get_args}()}.  Walks up the call stack
#' two levels to determine the name of the expression supplied as
#' \code{parm_value}.
#'
#' @param arg_position Integer >= 1.  Positional index of the argument whose
#'   name is wanted (1-based, excluding the function name itself).
#'
#' @return Character scalar, or \code{NA_character_} if the name cannot be
#'   determined.
#' @export
get_arg_name_from_caller <- function(arg_position = 1) {
  stopifnot(arg_position > 0L)
  call <- sys.call(-2)
  if (is.null(call)) return(NA_character_)
  call_list <- as.list(call)
  if (length(call_list) < arg_position + 1L) return(NA_character_)
  if (deparse(call_list[[1]]) %in% c("df_dp", "dv_dv")) {
    arg_position <- 2L
    if (length(call_list) < 3L) return(NA_character_)
  }
  full_expr <- deparse(call_list[[arg_position + 1L]])
  base_name <- sub("\\[.*$", "", full_expr)
  return(base_name)
}

# ---- hash_derivative_key (internal) -----------------------------------------

#' Compute a cache key for a (function, parameter, context) triple
#'
#' @description
#' Hashes the function body, current parameter value (and its dimensions),
#' and context arguments using \code{digest::digest(algo = "xxhash64")} to
#' produce a short string suitable as an environment key for the Jacobian
#' cache.
#'
#' @param funct Function whose Jacobian is being computed.
#' @param parm_value Current value of the differentiation parameter.
#' @param ctx A \code{DerivativeContext}.
#'
#' @return Character scalar (xxhash64 hex string).
#' @keywords internal
.hash_derivative_key <- function(funct, parm_value, ctx) {
  digest::digest(
    list(
      body      = body(funct),
      parm      = parm_value,
      parm_dim  = dim(parm_value),
      args      = as.list(ctx$derivative_args)
    ),
    algo = "xxhash64"
  )
}

# ---- unify_sparse_pattern (internal) ----------------------------------------

#' Unify the non-zero pattern of two sparse Matrix summaries
#'
#' @description
#' Takes two \code{summary(sparseMatrix)} data frames (\code{i}, \code{j},
#' \code{x} columns) and produces a \code{sparseMatrix} whose non-zero
#' pattern is the union, with values taken from \code{result_smry}.
#'
#' Used internally by \code{df_dp_sub()} when the function being
#' differentiated has a changing sparsity pattern across perturbations.
#'
#' @param template_smry Three-column summary data frame for the template.
#' @param result_smry   Three-column summary data frame for the current result.
#' @param dim_result    Integer vector of length 2 — dimensions of the matrix.
#'
#' @return A sparse \code{Matrix} with the unified non-zero pattern.
#' @keywords internal
.unify_sparse_pattern <- function(template_smry, result_smry, dim_result) {
  smry_union <- unique(rbind(
    template_smry[, c("i", "j")],
    result_smry[,  c("i", "j")]
  ))
  result <- Matrix::sparseMatrix(
    i    = smry_union$i,
    j    = smry_union$j,
    x    = rep(0, nrow(smry_union)),
    dims = dim_result
  )
  ij         <- as.matrix(result_smry[, c("i", "j")])
  result[ij] <- result_smry[, "x"]
  return(result)
}

# ---- df_dp_sub (internal) ---------------------------------------------------

#' Compute and cache the numerical Jacobian for a matrix-valued function
#'
#' @description
#' Core internal function used by \code{\link{df_dp}}.  Flattens
#' \code{parm_value} to a numeric vector (using only the stored non-zero
#' elements for sparse \code{Matrix} objects), runs
#' \code{numDeriv::jacobian()}, handles the case where the function result
#' changes its sparsity pattern across perturbations, and caches the result.
#'
#' @param funct A function \code{f(parm_value, ctx = ctx)} that returns a
#'   \code{Matrix} or numeric result.
#' @param parm_value Current value of the differentiation parameter (a
#'   \code{Matrix} for now; numeric support is a TODO).
#' @param ctx A \code{DerivativeContext}.
#'
#' @return A numeric matrix \code{J} of dimensions
#'   \code{(length(result@x), length(parm_vec))} with
#'   \code{attr(J, "result_template")} set to the structural template used.
#' @keywords internal
.df_dp_sub <- function(funct, parm_value, ctx) {
  if (!inherits(ctx, "DerivativeContext")) {
    stop(".df_dp_sub(): ctx must be a DerivativeContext")
  }
  # TODO: allow numeric parm_value (currently requires Matrix)
  if (!methods::is(parm_value, "Matrix")) {
    stop(".df_dp_sub(): parm_value must be a Matrix (numeric support is a TODO)")
  }
  parm_dim <- dim(parm_value)
  if (is.null(parm_dim)) stop(".df_dp_sub(): parm_value must have dim()")

  key <- .hash_derivative_key(funct, parm_value, ctx)
  if (exists(key, envir = ctx$jacobian_cache)) {
    return(invisible(get(key, envir = ctx$jacobian_cache)))
  }

  # Flatten sparse parameter to vector of stored non-zeroes
  parm_vec <- parm_value@x

  # Run funct once to get the result structure
  ctx$result_template <- funct(parm_value)

  # Wrapper for numDeriv::jacobian: accepts flat parm_vec, returns flat result
  wrapped_funct <- function(perturbed_parm_vec, ctx) {
    perturbed_parm_value        <- parm_value
    perturbed_parm_value@x      <- perturbed_parm_vec
    result                      <- funct(perturbed_parm_value, ctx = ctx)

    if (!isTRUE(all.equal(dim(ctx$result_template), dim(result)))) {
      stop("Result of funct() changed dimensions during jacobian().")
    }

    if (methods::is(result, "Matrix")) {
      # For sparse matrices only: check and (if needed) unify the non-zero
      # pattern, because the pattern must be consistent across all perturbations
      # so that the Jacobian columns are comparable.
      # Dense Matrix objects do not have a non-zero pattern to manage; just
      # flatten their values directly.
      if (methods::is(result, "sparseMatrix")) {
        # Summarise result to examine non-zero pattern
        smry_fn <- function(m) {
          if (all(dim(m) == c(1L, 1L))) {
            data.frame(i = 1L, j = 1L, x = as.numeric(m))
          } else {
            as.data.frame(Matrix::summary(m))
          }
        }
        result_smry   <- smry_fn(result)
        template_smry <- smry_fn(ctx$result_template)

        if (!isTRUE(all.equal(template_smry[, 1L:2L], result_smry[, 1L:2L]))) {
          # Sparsity pattern has changed — unify and possibly retry jacobian
          unified <- .unify_sparse_pattern(template_smry, result_smry, dim(result))
          if (methods::existsFunction("partMatrixFromTemplate")) {
            unified <- get("partMatrixFromTemplate")(unified, ctx$result_template)
          }
          if (length(unified@x) > nrow(template_smry)) {
            ctx$result_template      <- unified
            .nvd_env$last_deriv_ctx  <- ctx
            stop("Result of funct() added non-zero elements during jacobian().")
          } else {
            result <- unified
          }
        }
        result_vec <- result@x
      } else {
        # Dense Matrix — simply flatten to numeric vector
        result_vec <- as.numeric(result)
      }
    } else {
      result_vec <- as.numeric(result)
    }

    vec <- tryCatch(
      as.numeric(result_vec),
      error = function(e) stop("funct() output could not be coerced to numeric: ", conditionMessage(e))
    )
    return(vec)
  }

  # Sanity-check the wrapper
  wrapped_result <- wrapped_funct(parm_vec, ctx)
  stopifnot(is.numeric(wrapped_result), !methods::is(wrapped_result, "Matrix"))

  # Compute numerical Jacobian (with retry if sparsity pattern changes)
  J <- .run_jacobian(wrapped_funct, parm_vec, ctx)

  if (is.null(dim(J)) || length(dim(J)) != 2L) {
    dim(J) <- c(length(wrapped_funct(parm_value, ctx)), length(parm_vec))
  }

  attr(J, "result_template") <- ctx$result_template
  ctx$result_template        <- NULL
  assign(key, J, envir = ctx$jacobian_cache)
  return(J)
}

# ---- run_jacobian (internal) ------------------------------------------------

#' Run numDeriv::jacobian with automatic retry on changing sparsity
#'
#' @description
#' Calls \code{numDeriv::jacobian()} on \code{wrapped_funct} and handles the
#' \code{"added non-zero elements"} error by refreshing \code{ctx} from the
#' package environment and recursing.
#'
#' @param wrapped_funct Flat-to-flat wrapper function suitable for
#'   \code{numDeriv::jacobian}.
#' @param parm_value Flat numeric vector of parameter values.
#' @param ctx A \code{DerivativeContext}.
#'
#' @return Numeric Jacobian matrix.
#' @keywords internal
.run_jacobian <- function(wrapped_funct, parm_value, ctx) {
  J <- tryCatch({
    numDeriv::jacobian(func = wrapped_funct, x = parm_value, ctx = ctx)
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("Result of funct.* added non-zero elements during jacobian", msg)) {
      ctx_retry <- .nvd_env$last_deriv_ctx
      .run_jacobian(wrapped_funct, parm_value, ctx_retry)
    } else {
      stop("jacobian() failed: ", conditionMessage(e))
    }
  })
  return(J)
}

# ---- df_dp ------------------------------------------------------------------

#' Numerical derivative of a matrix-valued function wrt a scalar parameter
#'
#' @description
#' Computes (or retrieves from cache) the numerical Jacobian of \code{funct}
#' with respect to \code{parm_value}, then extracts the column corresponding
#' to element \code{[row, col]} of \code{parm_value} and formats it to match
#' the structure of \code{funct}'s output.
#'
#' @param funct A function \code{f(parm_value, ctx = ctx)} returning a
#'   \code{Matrix}.
#' @param parm_value The parameter matrix.
#' @param row,col Row and column index of the scalar element to differentiate
#'   with respect to.  May be \code{NULL} for 1-D or scalar parameters.
#' @param ctx A \code{DerivativeContext}.  Defaults to
#'   \code{\link{get_context}()}.
#'
#' @return A \code{Matrix} (or numeric) with the same structure as
#'   \code{funct}'s output, containing \eqn{\partial f / \partial
#'   \text{parm\_value}_{row,col}}.
#' @export
df_dp <- function(funct, parm_value, row = NULL, col = NULL,
                  ctx = get_context()) {
  args     <- get_args(ctx, parm_value)
  J        <- .df_dp_sub(funct, parm_value, ctx)
  parm_dim <- dim(parm_value)

  if (length(parm_dim) >= 1L && is.null(row) && parm_dim[1L] > 1L) {
    stop("df_dp(): 'row' must be specified for parm_value with multiple rows.")
  }
  if (length(parm_dim) >= 2L && is.null(col) && parm_dim[2L] > 1L) {
    stop("df_dp(): 'col' must be specified for parm_value with multiple columns.")
  }

  # Find position in @x vector
  indices <- parm_value
  indices@x <- as.double(seq_along(indices@x))
  idx <- if (length(dim(parm_value)) == 2L) {
    indices[row, col]
  } else if (length(dim(parm_value)) > 1L) {
    indices[row]
  } else {
    1L
  }

  result_vec      <- J[, as.numeric(idx)]
  result_template <- attr(J, "result_template")

  if (methods::is(result_template, "Matrix")) {
    result    <- result_template
    result@x  <- result_vec
  } else {
    result <- result_vec
  }
  return(result)
}

# ---- dv_dv ------------------------------------------------------------------

#' Jacobian of a vector-valued function wrt a vector parameter
#'
#' @description
#' Returns the full Jacobian matrix for a function that maps a numeric/Matrix
#' vector to another numeric/Matrix vector.  Both parameter and result must be
#' 1-dimensional (scalar or vector, not a 2-D matrix).
#'
#' @param funct A function \code{f(parm_value, ctx = ctx)}.
#' @param parm_value A 1-D numeric or \code{Matrix} vector.
#' @param ctx A \code{DerivativeContext}.  Defaults to
#'   \code{\link{get_context}()}.
#'
#' @return A numeric matrix of dimensions
#'   \code{(length(f(parm_value)), length(parm_value))}.
#' @export
dv_dv <- function(funct, parm_value, ctx = get_context()) {
  args             <- get_args(ctx, parm_value)
  parm_d           <- dim(parm_value)
  result_template  <- funct(parm_value)
  result_d         <- dim(result_template)
  stopifnot(is.null(parm_d)   || length(parm_d)   == 1L)
  stopifnot(is.null(result_d) || length(result_d) == 1L)

  J <- .df_dp_sub(funct, parm_value, ctx)
  stopifnot(nrow(J) == length(result_template), ncol(J) == length(parm_value))
  return(J)
}
