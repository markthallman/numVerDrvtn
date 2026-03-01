# Auto-generated PartitionedMatrix wrapper methods
# Generated: 2026-03-01 10:30:34.429653
# Generator version: 1.0.0
# Generator R version: R version 4.3.2 (2023-10-31 ucrt)
# Matrix version: 1.6.1.1

#' @rdname partitioned-arithmetic
#' @aliases !,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('!', signature(x = 'PartitionedMatrix'),
  function (x) {
    new('PartitionedMatrix', Mtrx = !(x@Mtrx), row_prtn = x@row_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases %%,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('%%', signature(e1 = 'PartitionedMatrix', e2 = 'PartitionedMatrix'),
  function (e1, e2) {
    verify_partitions_compatible(end_row(e1), end_row(e2))
    verify_partitions_compatible(end_col(e1), end_col(e2))
    new('PartitionedMatrix', Mtrx = e1@Mtrx %% e2@Mtrx, row_prtn = e1@row_prtn, col_prtn = e1@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases %%,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('%%', signature(e1 = 'ANY', e2 = 'PartitionedMatrix'),
  function (e1, e2) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(e1) && length(e1) == 1) || (methods::is(e1, 'Matrix') && nrow(e1) == 1 && ncol(e1) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('%%', list(e1, e2@Mtrx))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = e2@row_prtn, col_prtn = e2@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(e1), error = function(e) stop(sprintf('%%: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('%%', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(coerced_arg, e2)
  })

#' @rdname partitioned-arithmetic
#' @aliases %%,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('%%', signature(e1 = 'PartitionedMatrix', e2 = 'ANY'),
  function (e1, e2) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(e2) && length(e2) == 1) || (methods::is(e2, 'Matrix') && nrow(e2) == 1 && ncol(e2) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('%%', list(e1@Mtrx, e2))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = e1@row_prtn, col_prtn = e1@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(e2), error = function(e) stop(sprintf('%%: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('%%', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(e1, coerced_arg)
  })

#' @rdname partitioned-arithmetic
#' @aliases %*%,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('%*%', signature(x = 'PartitionedMatrix', y = 'PartitionedMatrix'),
  function (x, y) {
    verify_partitions_compatible(end_col(x), end_row(y))
    new('PartitionedMatrix', Mtrx = x@Mtrx %*% y@Mtrx, row_prtn = x@row_prtn, col_prtn = y@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases %*%,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('%*%', signature(x = 'ANY', y = 'PartitionedMatrix'),
  function (x, y) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(x) && length(x) == 1) || (methods::is(x, 'Matrix') && nrow(x) == 1 && ncol(x) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('%*%', list(x, y@Mtrx))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = y@row_prtn, col_prtn = y@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(x), error = function(e) stop(sprintf('%*%: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('%*%', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(coerced_arg, y)
  })

#' @rdname partitioned-arithmetic
#' @aliases %*%,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('%*%', signature(x = 'PartitionedMatrix', y = 'ANY'),
  function (x, y) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(y) && length(y) == 1) || (methods::is(y, 'Matrix') && nrow(y) == 1 && ncol(y) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('%*%', list(x@Mtrx, y))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = x@row_prtn, col_prtn = x@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(y), error = function(e) stop(sprintf('%*%: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('%*%', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(x, coerced_arg)
  })

#' @rdname partitioned-arithmetic
#' @aliases %/%,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('%/%', signature(e1 = 'PartitionedMatrix', e2 = 'PartitionedMatrix'),
  function (e1, e2) {
    verify_partitions_compatible(end_row(e1), end_row(e2))
    verify_partitions_compatible(end_col(e1), end_col(e2))
    new('PartitionedMatrix', Mtrx = e1@Mtrx %/% e2@Mtrx, row_prtn = e1@row_prtn, col_prtn = e1@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases %/%,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('%/%', signature(e1 = 'ANY', e2 = 'PartitionedMatrix'),
  function (e1, e2) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(e1) && length(e1) == 1) || (methods::is(e1, 'Matrix') && nrow(e1) == 1 && ncol(e1) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('%/%', list(e1, e2@Mtrx))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = e2@row_prtn, col_prtn = e2@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(e1), error = function(e) stop(sprintf('%/%: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('%/%', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(coerced_arg, e2)
  })

#' @rdname partitioned-arithmetic
#' @aliases %/%,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('%/%', signature(e1 = 'PartitionedMatrix', e2 = 'ANY'),
  function (e1, e2) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(e2) && length(e2) == 1) || (methods::is(e2, 'Matrix') && nrow(e2) == 1 && ncol(e2) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('%/%', list(e1@Mtrx, e2))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = e1@row_prtn, col_prtn = e1@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(e2), error = function(e) stop(sprintf('%/%: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('%/%', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(e1, coerced_arg)
  })

#' @rdname partitioned-arithmetic
#' @aliases &,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('&', signature(e1 = 'PartitionedMatrix', e2 = 'PartitionedMatrix'),
  function (e1, e2) {
    verify_partitions_compatible(end_row(e1), end_row(e2))
    verify_partitions_compatible(end_col(e1), end_col(e2))
    new('PartitionedMatrix', Mtrx = e1@Mtrx & e2@Mtrx, row_prtn = e1@row_prtn, col_prtn = e1@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases &,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('&', signature(e1 = 'ANY', e2 = 'PartitionedMatrix'),
  function (e1, e2) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(e1) && length(e1) == 1) || (methods::is(e1, 'Matrix') && nrow(e1) == 1 && ncol(e1) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('&', list(e1, e2@Mtrx))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = e2@row_prtn, col_prtn = e2@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(e1), error = function(e) stop(sprintf('&: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('&', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(coerced_arg, e2)
  })

#' @rdname partitioned-arithmetic
#' @aliases &,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('&', signature(e1 = 'PartitionedMatrix', e2 = 'ANY'),
  function (e1, e2) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(e2) && length(e2) == 1) || (methods::is(e2, 'Matrix') && nrow(e2) == 1 && ncol(e2) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('&', list(e1@Mtrx, e2))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = e1@row_prtn, col_prtn = e1@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(e2), error = function(e) stop(sprintf('&: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('&', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(e1, coerced_arg)
  })

#' @rdname partitioned-arithmetic
#' @aliases *,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('*', signature(e1 = 'PartitionedMatrix', e2 = 'PartitionedMatrix'),
  function (e1, e2) {
    verify_partitions_compatible(end_row(e1), end_row(e2))
    verify_partitions_compatible(end_col(e1), end_col(e2))
    new('PartitionedMatrix', Mtrx = e1@Mtrx * e2@Mtrx, row_prtn = e1@row_prtn, col_prtn = e1@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases *,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('*', signature(e1 = 'ANY', e2 = 'PartitionedMatrix'),
  function (e1, e2) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(e1) && length(e1) == 1) || (methods::is(e1, 'Matrix') && nrow(e1) == 1 && ncol(e1) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('*', list(e1, e2@Mtrx))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = e2@row_prtn, col_prtn = e2@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(e1), error = function(e) stop(sprintf('*: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('*', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(coerced_arg, e2)
  })

#' @rdname partitioned-arithmetic
#' @aliases *,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('*', signature(e1 = 'PartitionedMatrix', e2 = 'ANY'),
  function (e1, e2) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(e2) && length(e2) == 1) || (methods::is(e2, 'Matrix') && nrow(e2) == 1 && ncol(e2) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('*', list(e1@Mtrx, e2))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = e1@row_prtn, col_prtn = e1@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(e2), error = function(e) stop(sprintf('*: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('*', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(e1, coerced_arg)
  })

#' @rdname partitioned-arithmetic
#' @aliases /,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('/', signature(e1 = 'PartitionedMatrix', e2 = 'PartitionedMatrix'),
  function (e1, e2) {
    verify_partitions_compatible(end_row(e1), end_row(e2))
    verify_partitions_compatible(end_col(e1), end_col(e2))
    new('PartitionedMatrix', Mtrx = e1@Mtrx / e2@Mtrx, row_prtn = e1@row_prtn, col_prtn = e1@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases /,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('/', signature(e1 = 'ANY', e2 = 'PartitionedMatrix'),
  function (e1, e2) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(e1) && length(e1) == 1) || (methods::is(e1, 'Matrix') && nrow(e1) == 1 && ncol(e1) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('/', list(e1, e2@Mtrx))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = e2@row_prtn, col_prtn = e2@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(e1), error = function(e) stop(sprintf('/: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('/', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(coerced_arg, e2)
  })

#' @rdname partitioned-arithmetic
#' @aliases /,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('/', signature(e1 = 'PartitionedMatrix', e2 = 'ANY'),
  function (e1, e2) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(e2) && length(e2) == 1) || (methods::is(e2, 'Matrix') && nrow(e2) == 1 && ncol(e2) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('/', list(e1@Mtrx, e2))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = e1@row_prtn, col_prtn = e1@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(e2), error = function(e) stop(sprintf('/: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('/', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(e1, coerced_arg)
  })

#' @rdname partitioned-arithmetic
#' @aliases ^,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('^', signature(e1 = 'PartitionedMatrix', e2 = 'PartitionedMatrix'),
  function (e1, e2) {
    verify_partitions_compatible(end_row(e1), end_row(e2))
    verify_partitions_compatible(end_col(e1), end_col(e2))
    new('PartitionedMatrix', Mtrx = e1@Mtrx ^ e2@Mtrx, row_prtn = e1@row_prtn, col_prtn = e1@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases ^,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('^', signature(e1 = 'ANY', e2 = 'PartitionedMatrix'),
  function (e1, e2) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(e1) && length(e1) == 1) || (methods::is(e1, 'Matrix') && nrow(e1) == 1 && ncol(e1) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('^', list(e1, e2@Mtrx))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = e2@row_prtn, col_prtn = e2@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(e1), error = function(e) stop(sprintf('^: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('^', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(coerced_arg, e2)
  })

#' @rdname partitioned-arithmetic
#' @aliases ^,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('^', signature(e1 = 'PartitionedMatrix', e2 = 'ANY'),
  function (e1, e2) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(e2) && length(e2) == 1) || (methods::is(e2, 'Matrix') && nrow(e2) == 1 && ncol(e2) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('^', list(e1@Mtrx, e2))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = e1@row_prtn, col_prtn = e1@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(e2), error = function(e) stop(sprintf('^: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('^', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(e1, coerced_arg)
  })

#' @rdname partitioned-arithmetic
#' @aliases +,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('+', signature(e1 = 'PartitionedMatrix', e2 = 'PartitionedMatrix'),
  function (e1, e2) {
    verify_partitions_compatible(end_row(e1), end_row(e2))
    verify_partitions_compatible(end_col(e1), end_col(e2))
    new('PartitionedMatrix', Mtrx = e1@Mtrx + e2@Mtrx, row_prtn = e1@row_prtn, col_prtn = e1@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases +,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('+', signature(e1 = 'ANY', e2 = 'PartitionedMatrix'),
  function (e1, e2) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(e1) && length(e1) == 1) || (methods::is(e1, 'Matrix') && nrow(e1) == 1 && ncol(e1) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('+', list(e1, e2@Mtrx))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = e2@row_prtn, col_prtn = e2@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(e1), error = function(e) stop(sprintf('+: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('+', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(coerced_arg, e2)
  })

#' @rdname partitioned-arithmetic
#' @aliases +,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('+', signature(e1 = 'PartitionedMatrix', e2 = 'ANY'),
  function (e1, e2) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(e2) && length(e2) == 1) || (methods::is(e2, 'Matrix') && nrow(e2) == 1 && ncol(e2) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('+', list(e1@Mtrx, e2))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = e1@row_prtn, col_prtn = e1@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(e2), error = function(e) stop(sprintf('+: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('+', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(e1, coerced_arg)
  })

#' @rdname partitioned-arithmetic
#' @aliases +,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('+', signature(e1 = 'PartitionedMatrix', e2 = 'missing'),
  function (e1, e2) {
    new('PartitionedMatrix', Mtrx = +(e1@Mtrx), row_prtn = e1@row_prtn, col_prtn = e1@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases anyNA,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('anyNA', signature(x = 'PartitionedMatrix'),
  function (x, recursive = FALSE) {
    anyNA(x@Mtrx, recursive)
})

#' @rdname partitioned-arithmetic
#' @aliases band,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('band', signature(x = 'PartitionedMatrix'),
  function (x, k1, k2, ...) {
    new('PartitionedMatrix', Mtrx = band(x@Mtrx, k1, k2, ...), row_prtn = x@row_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases cov2cor,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('cov2cor', signature(V = 'PartitionedMatrix'),
  function (V) {
    new('PartitionedMatrix', Mtrx = cov2cor(V@Mtrx), row_prtn = V@row_prtn, col_prtn = V@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases crossprod,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('crossprod', signature(x = 'PartitionedMatrix', y = 'PartitionedMatrix'),
  function (x, y = NULL, ...) {
    verify_partitions_compatible(end_row(x), end_row(y))
    new('PartitionedMatrix', Mtrx = crossprod(x@Mtrx, y@Mtrx, ...), row_prtn = x@col_prtn, col_prtn = y@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases crossprod,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('crossprod', signature(x = 'ANY', y = 'PartitionedMatrix'),
  function (x, y = NULL, ...) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(x) && length(x) == 1) || (methods::is(x, 'Matrix') && nrow(x) == 1 && ncol(x) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('crossprod', list(x, y@Mtrx))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = y@row_prtn, col_prtn = y@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(x), error = function(e) stop(sprintf('crossprod: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('crossprod', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(coerced_arg, y, ...)
  })

#' @rdname partitioned-arithmetic
#' @aliases crossprod,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('crossprod', signature(x = 'PartitionedMatrix', y = 'ANY'),
  function (x, y = NULL, ...) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(y) && length(y) == 1) || (methods::is(y, 'Matrix') && nrow(y) == 1 && ncol(y) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('crossprod', list(x@Mtrx, y))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = x@row_prtn, col_prtn = x@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(y), error = function(e) stop(sprintf('crossprod: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('crossprod', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(x, coerced_arg, ...)
  })

#' @rdname partitioned-arithmetic
#' @aliases crossprod,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('crossprod', signature(x = 'PartitionedMatrix', y = 'missing'),
  function (x, y = NULL, ...) {
    new('PartitionedMatrix', Mtrx = crossprod(x@Mtrx, ...), row_prtn = x@col_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases diag<-,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('diag<-', signature(x = 'PartitionedMatrix'),
  function (x, value) {
    `diag`(x@Mtrx) <- value
    invisible(x)
})

#' @rdname partitioned-arithmetic
#' @aliases dim,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('dim', signature(x = 'PartitionedMatrix'),
  function (x) {
    dim(x@Mtrx)
})

#' @rdname partitioned-arithmetic
#' @aliases dim<-,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('dim<-', signature(x = 'PartitionedMatrix'),
  function (x, value) {
    `dim`(x@Mtrx) <- value
    invisible(x)
})

#' @rdname partitioned-arithmetic
#' @aliases dimnames,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('dimnames', signature(x = 'PartitionedMatrix'),
  function (x) {
    dimnames(x@Mtrx)
})

#' @rdname partitioned-arithmetic
#' @aliases dimnames<-,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('dimnames<-', signature(x = 'PartitionedMatrix', value = 'list'),
  function (x, value) {
    `dimnames`(x@Mtrx) <- value
    invisible(x)
})

#' @rdname partitioned-arithmetic
#' @aliases expand,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('expand', signature(x = 'PartitionedMatrix'),
  function (x, ...) {
    new('PartitionedMatrix', Mtrx = expand(x@Mtrx, ...), row_prtn = x@row_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases expand1,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('expand1', signature(x = 'PartitionedMatrix'),
  function (x, which, ...) {
    new('PartitionedMatrix', Mtrx = expand1(x@Mtrx, which, ...), row_prtn = x@row_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases expand2,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('expand2', signature(x = 'PartitionedMatrix'),
  function (x, ...) {
    new('PartitionedMatrix', Mtrx = expand2(x@Mtrx, ...), row_prtn = x@row_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases expm,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('expm', signature(x = 'PartitionedMatrix'),
  function (x) {
    new('PartitionedMatrix', Mtrx = expm(x@Mtrx), row_prtn = x@row_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases format,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('format', signature(x = 'PartitionedMatrix'),
  function (x, ...) {
    format(x@Mtrx, ...)
})

#' @rdname partitioned-arithmetic
#' @aliases head,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('head', signature(x = 'PartitionedMatrix'),
  function (x, ...) {
    new('PartitionedMatrix', Mtrx = head(x@Mtrx, ...), row_prtn = x@row_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases image,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('image', signature(x = 'PartitionedMatrix'),
  function (x, ...) {
    image(x@Mtrx, ...)
})

#' @rdname partitioned-arithmetic
#' @aliases is.finite,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('is.finite', signature(x = 'PartitionedMatrix'),
  function (x) {
    new('PartitionedMatrix', Mtrx = is.finite(x@Mtrx), row_prtn = x@row_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases is.infinite,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('is.infinite', signature(x = 'PartitionedMatrix'),
  function (x) {
    new('PartitionedMatrix', Mtrx = is.infinite(x@Mtrx), row_prtn = x@row_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases is.na,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('is.na', signature(x = 'PartitionedMatrix'),
  function (x) {
    new('PartitionedMatrix', Mtrx = is.na(x@Mtrx), row_prtn = x@row_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases is.nan,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('is.nan', signature(x = 'PartitionedMatrix'),
  function (x) {
    new('PartitionedMatrix', Mtrx = is.nan(x@Mtrx), row_prtn = x@row_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases isDiagonal,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('isDiagonal', signature(object = 'PartitionedMatrix'),
  function (object) {
    isDiagonal(object@Mtrx)
})

#' @rdname partitioned-arithmetic
#' @aliases isSymmetric,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('isSymmetric', signature(object = 'PartitionedMatrix'),
  function (object, ...) {
    isSymmetric(object@Mtrx, ...)
})

#' @rdname partitioned-arithmetic
#' @aliases isTriangular,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('isTriangular', signature(object = 'PartitionedMatrix'),
  function (object, upper = NA, ...) {
    isTriangular(object@Mtrx, upper, ...)
})

#' @rdname partitioned-arithmetic
#' @aliases length,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('length', signature(x = 'PartitionedMatrix'),
  function (x) {
    length(x@Mtrx)
})

#' @rdname partitioned-arithmetic
#' @aliases log,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('log', signature(x = 'PartitionedMatrix'),
  function (x, ...) {
    new('PartitionedMatrix', Mtrx = log(x@Mtrx, ...), row_prtn = x@row_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases pack,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('pack', signature(x = 'PartitionedMatrix'),
  function (x, ...) {
    new('PartitionedMatrix', Mtrx = pack(x@Mtrx, ...), row_prtn = x@row_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases qr.coef,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('qr.coef', signature(qr = 'PartitionedMatrix', y = 'PartitionedMatrix'),
  function (qr, y) {
    verify_partitions_compatible(end_row(qr), end_row(y))
    verify_partitions_compatible(end_col(qr), end_col(y))
    new('PartitionedMatrix', Mtrx = qr.coef(qr@Mtrx, y@Mtrx), row_prtn = qr@row_prtn, col_prtn = qr@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases qr.coef,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('qr.coef', signature(qr = 'ANY', y = 'PartitionedMatrix'),
  function (qr, y) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(qr) && length(qr) == 1) || (methods::is(qr, 'Matrix') && nrow(qr) == 1 && ncol(qr) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('qr.coef', list(qr, y@Mtrx))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = y@row_prtn, col_prtn = y@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(qr), error = function(e) stop(sprintf('qr.coef: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('qr.coef', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(coerced_arg, y)
  })

#' @rdname partitioned-arithmetic
#' @aliases qr.coef,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('qr.coef', signature(qr = 'PartitionedMatrix', y = 'ANY'),
  function (qr, y) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(y) && length(y) == 1) || (methods::is(y, 'Matrix') && nrow(y) == 1 && ncol(y) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('qr.coef', list(qr@Mtrx, y))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = qr@row_prtn, col_prtn = qr@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(y), error = function(e) stop(sprintf('qr.coef: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('qr.coef', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(qr, coerced_arg)
  })

#' @rdname partitioned-arithmetic
#' @aliases qr.fitted,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('qr.fitted', signature(qr = 'PartitionedMatrix', y = 'PartitionedMatrix'),
  function (qr, y, k = qr$rank) {
    verify_partitions_compatible(end_row(qr), end_row(y))
    verify_partitions_compatible(end_col(qr), end_col(y))
    new('PartitionedMatrix', Mtrx = qr.fitted(qr@Mtrx, y@Mtrx, k), row_prtn = qr@row_prtn, col_prtn = qr@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases qr.fitted,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('qr.fitted', signature(qr = 'ANY', y = 'PartitionedMatrix'),
  function (qr, y, k = qr$rank) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(qr) && length(qr) == 1) || (methods::is(qr, 'Matrix') && nrow(qr) == 1 && ncol(qr) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('qr.fitted', list(qr, y@Mtrx))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = y@row_prtn, col_prtn = y@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(qr), error = function(e) stop(sprintf('qr.fitted: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('qr.fitted', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(coerced_arg, y, k)
  })

#' @rdname partitioned-arithmetic
#' @aliases qr.fitted,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('qr.fitted', signature(qr = 'PartitionedMatrix', y = 'ANY'),
  function (qr, y, k = qr$rank) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(y) && length(y) == 1) || (methods::is(y, 'Matrix') && nrow(y) == 1 && ncol(y) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('qr.fitted', list(qr@Mtrx, y))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = qr@row_prtn, col_prtn = qr@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(y), error = function(e) stop(sprintf('qr.fitted: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('qr.fitted', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(qr, coerced_arg, k)
  })

#' @rdname partitioned-arithmetic
#' @aliases qr.Q,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('qr.Q', signature(qr = 'PartitionedMatrix'),
  function (qr, complete = FALSE, Dvec) {
    new('PartitionedMatrix', Mtrx = qr.Q(qr@Mtrx, complete, Dvec), row_prtn = qr@row_prtn, col_prtn = qr@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases qr.qty,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('qr.qty', signature(qr = 'PartitionedMatrix', y = 'PartitionedMatrix'),
  function (qr, y) {
    verify_partitions_compatible(end_row(qr), end_row(y))
    verify_partitions_compatible(end_col(qr), end_col(y))
    new('PartitionedMatrix', Mtrx = qr.qty(qr@Mtrx, y@Mtrx), row_prtn = qr@row_prtn, col_prtn = qr@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases qr.qty,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('qr.qty', signature(qr = 'ANY', y = 'PartitionedMatrix'),
  function (qr, y) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(qr) && length(qr) == 1) || (methods::is(qr, 'Matrix') && nrow(qr) == 1 && ncol(qr) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('qr.qty', list(qr, y@Mtrx))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = y@row_prtn, col_prtn = y@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(qr), error = function(e) stop(sprintf('qr.qty: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('qr.qty', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(coerced_arg, y)
  })

#' @rdname partitioned-arithmetic
#' @aliases qr.qty,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('qr.qty', signature(qr = 'PartitionedMatrix', y = 'ANY'),
  function (qr, y) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(y) && length(y) == 1) || (methods::is(y, 'Matrix') && nrow(y) == 1 && ncol(y) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('qr.qty', list(qr@Mtrx, y))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = qr@row_prtn, col_prtn = qr@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(y), error = function(e) stop(sprintf('qr.qty: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('qr.qty', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(qr, coerced_arg)
  })

#' @rdname partitioned-arithmetic
#' @aliases qr.qy,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('qr.qy', signature(qr = 'PartitionedMatrix', y = 'PartitionedMatrix'),
  function (qr, y) {
    verify_partitions_compatible(end_row(qr), end_row(y))
    verify_partitions_compatible(end_col(qr), end_col(y))
    new('PartitionedMatrix', Mtrx = qr.qy(qr@Mtrx, y@Mtrx), row_prtn = qr@row_prtn, col_prtn = qr@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases qr.qy,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('qr.qy', signature(qr = 'ANY', y = 'PartitionedMatrix'),
  function (qr, y) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(qr) && length(qr) == 1) || (methods::is(qr, 'Matrix') && nrow(qr) == 1 && ncol(qr) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('qr.qy', list(qr, y@Mtrx))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = y@row_prtn, col_prtn = y@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(qr), error = function(e) stop(sprintf('qr.qy: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('qr.qy', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(coerced_arg, y)
  })

#' @rdname partitioned-arithmetic
#' @aliases qr.qy,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('qr.qy', signature(qr = 'PartitionedMatrix', y = 'ANY'),
  function (qr, y) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(y) && length(y) == 1) || (methods::is(y, 'Matrix') && nrow(y) == 1 && ncol(y) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('qr.qy', list(qr@Mtrx, y))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = qr@row_prtn, col_prtn = qr@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(y), error = function(e) stop(sprintf('qr.qy: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('qr.qy', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(qr, coerced_arg)
  })

#' @rdname partitioned-arithmetic
#' @aliases qr.R,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('qr.R', signature(qr = 'PartitionedMatrix'),
  function (qr, complete = FALSE, ...) {
    new('PartitionedMatrix', Mtrx = qr.R(qr@Mtrx, complete, ...), row_prtn = qr@row_prtn, col_prtn = qr@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases qr.resid,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('qr.resid', signature(qr = 'PartitionedMatrix', y = 'PartitionedMatrix'),
  function (qr, y) {
    verify_partitions_compatible(end_row(qr), end_row(y))
    verify_partitions_compatible(end_col(qr), end_col(y))
    new('PartitionedMatrix', Mtrx = qr.resid(qr@Mtrx, y@Mtrx), row_prtn = qr@row_prtn, col_prtn = qr@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases qr.resid,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('qr.resid', signature(qr = 'ANY', y = 'PartitionedMatrix'),
  function (qr, y) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(qr) && length(qr) == 1) || (methods::is(qr, 'Matrix') && nrow(qr) == 1 && ncol(qr) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('qr.resid', list(qr, y@Mtrx))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = y@row_prtn, col_prtn = y@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(qr), error = function(e) stop(sprintf('qr.resid: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('qr.resid', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(coerced_arg, y)
  })

#' @rdname partitioned-arithmetic
#' @aliases qr.resid,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('qr.resid', signature(qr = 'PartitionedMatrix', y = 'ANY'),
  function (qr, y) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(y) && length(y) == 1) || (methods::is(y, 'Matrix') && nrow(y) == 1 && ncol(y) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('qr.resid', list(qr@Mtrx, y))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = qr@row_prtn, col_prtn = qr@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(y), error = function(e) stop(sprintf('qr.resid: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('qr.resid', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(qr, coerced_arg)
  })

#' @rdname partitioned-arithmetic
#' @aliases qr.X,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('qr.X', signature(qr = 'PartitionedMatrix'),
  function (qr, complete = FALSE, ncol, ...) {
    new('PartitionedMatrix', Mtrx = qr.X(qr@Mtrx, complete, ncol, ...), row_prtn = qr@row_prtn, col_prtn = qr@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases qr,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('qr', signature(x = 'PartitionedMatrix'),
  function (x, ...) {
    new('PartitionedMatrix', Mtrx = qr(x@Mtrx, ...), row_prtn = x@row_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases skewpart,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('skewpart', signature(x = 'PartitionedMatrix'),
  function (x) {
    new('PartitionedMatrix', Mtrx = skewpart(x@Mtrx), row_prtn = x@col_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases solve,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('solve', signature(a = 'PartitionedMatrix', b = 'PartitionedMatrix'),
  function (a, b, ...) {
    verify_partitions_compatible(end_row(a), end_row(b))
    new('PartitionedMatrix', Mtrx = solve(a@Mtrx, b@Mtrx, ...), row_prtn = a@col_prtn, col_prtn = b@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases solve,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('solve', signature(a = 'ANY', b = 'PartitionedMatrix'),
  function (a, b, ...) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(a) && length(a) == 1) || (methods::is(a, 'Matrix') && nrow(a) == 1 && ncol(a) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('solve', list(a, b@Mtrx))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = b@row_prtn, col_prtn = b@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(a), error = function(e) stop(sprintf('solve: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('solve', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(coerced_arg, b, ...)
  })

#' @rdname partitioned-arithmetic
#' @aliases solve,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('solve', signature(a = 'PartitionedMatrix', b = 'ANY'),
  function (a, b, ...) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(b) && length(b) == 1) || (methods::is(b, 'Matrix') && nrow(b) == 1 && ncol(b) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('solve', list(a@Mtrx, b))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = a@row_prtn, col_prtn = a@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(b), error = function(e) stop(sprintf('solve: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('solve', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(a, coerced_arg, ...)
  })

#' @rdname partitioned-arithmetic
#' @aliases solve,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('solve', signature(a = 'PartitionedMatrix', b = 'missing'),
  function (a, b, ...) {
    new('PartitionedMatrix', Mtrx = solve(a@Mtrx, ...), row_prtn = a@row_prtn, col_prtn = a@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases summary,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('summary', signature(object = 'PartitionedMatrix'),
  function (object, ...) {
    summary(object@Mtrx, ...)
})

#' @rdname partitioned-arithmetic
#' @aliases symmpart,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('symmpart', signature(x = 'PartitionedMatrix'),
  function (x) {
    new('PartitionedMatrix', Mtrx = symmpart(x@Mtrx), row_prtn = x@col_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases t,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('t', signature(x = 'PartitionedMatrix'),
  function (x) {
    new('PartitionedMatrix', Mtrx = t(x@Mtrx), row_prtn = x@col_prtn, col_prtn = x@row_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases tail,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('tail', signature(x = 'PartitionedMatrix'),
  function (x, ...) {
    new('PartitionedMatrix', Mtrx = tail(x@Mtrx, ...), row_prtn = x@row_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases tcrossprod,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('tcrossprod', signature(x = 'PartitionedMatrix', y = 'PartitionedMatrix'),
  function (x, y = NULL, ...) {
    verify_partitions_compatible(end_col(x), end_col(y))
    new('PartitionedMatrix', Mtrx = tcrossprod(x@Mtrx, y@Mtrx, ...), row_prtn = x@row_prtn, col_prtn = y@row_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases tcrossprod,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('tcrossprod', signature(x = 'ANY', y = 'PartitionedMatrix'),
  function (x, y = NULL, ...) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(x) && length(x) == 1) || (methods::is(x, 'Matrix') && nrow(x) == 1 && ncol(x) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('tcrossprod', list(x, y@Mtrx))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = y@row_prtn, col_prtn = y@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(x), error = function(e) stop(sprintf('tcrossprod: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('tcrossprod', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(coerced_arg, y, ...)
  })

#' @rdname partitioned-arithmetic
#' @aliases tcrossprod,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('tcrossprod', signature(x = 'PartitionedMatrix', y = 'ANY'),
  function (x, y = NULL, ...) {
    # Handle scalars specially - they broadcast without partition checking
    is_scalar <- (is.atomic(y) && length(y) == 1) || (methods::is(y, 'Matrix') && nrow(y) == 1 && ncol(y) == 1)
    if (is_scalar) {
      result_mtrx <- do.call('tcrossprod', list(x@Mtrx, y))
      return(new('PartitionedMatrix', Mtrx = result_mtrx, row_prtn = x@row_prtn, col_prtn = x@col_prtn))
    }
    # Non-scalar: coerce and use normal method
    coerced_arg <- tryCatch(as.PartitionedMatrix(y), error = function(e) stop(sprintf('tcrossprod: cannot coerce argument to PartitionedMatrix: %s', e$message), call. = FALSE))
    coerced_method <- methods::selectMethod('tcrossprod', c('PartitionedMatrix', 'PartitionedMatrix'))
    coerced_method(x, coerced_arg, ...)
  })

#' @rdname partitioned-arithmetic
#' @aliases tcrossprod,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('tcrossprod', signature(x = 'PartitionedMatrix', y = 'missing'),
  function (x, y = NULL, ...) {
    new('PartitionedMatrix', Mtrx = tcrossprod(x@Mtrx, ...), row_prtn = x@row_prtn, col_prtn = x@row_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases tril,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('tril', signature(x = 'PartitionedMatrix'),
  function (x, k = 0, ...) {
    new('PartitionedMatrix', Mtrx = tril(x@Mtrx, k, ...), row_prtn = x@row_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases triu,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('triu', signature(x = 'PartitionedMatrix'),
  function (x, k = 0, ...) {
    new('PartitionedMatrix', Mtrx = triu(x@Mtrx, k, ...), row_prtn = x@row_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases unpack,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('unpack', signature(x = 'PartitionedMatrix'),
  function (x, ...) {
    new('PartitionedMatrix', Mtrx = unpack(x@Mtrx, ...), row_prtn = x@row_prtn, col_prtn = x@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases update,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('update', signature(object = 'PartitionedMatrix'),
  function (object, ...) {
    new('PartitionedMatrix', Mtrx = update(object@Mtrx, ...), row_prtn = object@row_prtn, col_prtn = object@col_prtn)
})

#' @rdname partitioned-arithmetic
#' @aliases zapsmall,PartitionedMatrix,PartitionedMatrix-method
#' @export
setMethod('zapsmall', signature(x = 'PartitionedMatrix'),
  function (x, digits = getOption("digits")) {
    new('PartitionedMatrix', Mtrx = zapsmall(x@Mtrx, digits), row_prtn = x@row_prtn, col_prtn = x@col_prtn)
})


# Summary of generation
# Generated 64 core method(s)
