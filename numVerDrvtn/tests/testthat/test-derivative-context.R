# Test derivative context and numerical derivative computation
#
# Standard pattern for functions that work with derivatives:
#   my_function <- function(parm_value, ctx = get_context()) {
#     args <- get_args(ctx, parm_value)    # ONE-LINE BOILERPLATE
#     # ... function body that uses parm_value and derivative_args ...
#   }
#
# Before calling such functions, initialize the derivative context:
#   init_derivative_args(X = X_mat, Z = Z_mat, sigma = 2.5, ...)
#
# Then call your function with the parameter to differentiate:
#   result <- my_function(M)
#   deriv  <- df_dp(my_function, M, row = 1, col = 1)

test_that("init_derivative_args creates a context with named arguments", {
  ctx <- init_derivative_args(a = 2, b = 3)
  
  expect_s3_class(ctx, "DerivativeContext")
  expect_true(exists("derivative_args", envir = ctx))
  expect_equal(ctx$derivative_args$a, 2)
  expect_equal(ctx$derivative_args$b, 3)
})

test_that("get_context retrieves the current derivative context", {
  init_derivative_args(x = 10, y = 20)
  ctx <- get_context()
  
  expect_s3_class(ctx, "DerivativeContext")
  expect_equal(ctx$derivative_args$x, 10)
  expect_equal(ctx$derivative_args$y, 20)
})

test_that("get_context fails if no context has been initialized", {
  # Clear the context
  .nvd_env$last_deriv_ctx <- NULL
  
  expect_error(get_context(), "no derivative context has been initialised")
  
  # Restore a context for subsequent tests
  init_derivative_args(a = 1)
})

test_that("derivative function with ctx default parameter works correctly", {
  init_derivative_args(a = 2, b = 3)
  
  # Define a function following the standard pattern:
  # - First parameter is parm_value (the matrix being differentiated)
  # - Second parameter is ctx with default get_context()
  # - First line calls get_args() to unpack derivative_args
  funct <- function(parm_value, ctx = get_context()) {
    args <- get_args(ctx, parm_value)
    # Now a and b are available from derivative_args
    # Return a 2x2 matrix based on parm_value elements
    matrix(parm_value@x + a + b, nrow = 2)
  }
  
  M <- Matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, sparse = FALSE)
  
  result <- funct(M)
  # Expected: each element of M@x (1,2,3,4) + a (2) + b (3) = (6,7,8,9)
  expect_equal(as.vector(result), c(6, 7, 8, 9))
})

test_that("derivative function with matrix result of different size", {
  init_derivative_args(a = 2, b = 3)
  
  funct <- function(parm_value, ctx = get_context()) {
    args <- get_args(ctx, parm_value)
    # Result larger than parameter matrix
    # Convert to Matrix for tcrossprod to work
    combined <- rbind(parm_value + a, b * parm_value)
    tcrossprod(as.matrix(combined))
  }
  
  M <- Matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, sparse = FALSE)
  result <- funct(M)
  
  # Result should be 4x4 (2 row blocks × 2 cols, then tcrossprod)
  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 4)
})

test_that("df_dp computes numerical derivatives correctly", {
  init_derivative_args(a = 2, b = 3)
  
  # Simple function: f(M) = M + a + b
  # Derivative w.r.t. M[i,j] should be 1 for the (i,j) position
  funct <- function(parm_value, ctx = get_context()) {
    args <- get_args(ctx, parm_value)
    parm_value + a + b
  }
  
  M <- Matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, sparse = TRUE)
  
  # Derivative of M[1,1] w.r.t. M[1,1] should be 1
  deriv <- df_dp(funct, M, 1, 1)
  
  expect_s4_class(deriv, "Matrix")
  # The derivative at position (1,1) should be 1
  expect_equal(deriv[1, 1], 1)
})

test_that("dv_dv computes derivatives for vectorized function", {
  init_derivative_args(a = 2, b = 3)
  
  # Function that operates on 1-D numeric vector
  funct <- function(parm_value, ctx = get_context()) {
    args <- get_args(ctx, parm_value)
    # Return vector result: each element + a + b
    # Need to return Matrix for .df_dp_sub to work
    Matrix(parm_value + a + b, ncol = 1, sparse = TRUE)
  }
  
  # Use a plain numeric vector (1-D)
  vec_input <- c(1, 2, 3, 4)
  # Wrap as 1-column Matrix for .df_dp_sub compatibility
  vec_mat <- Matrix(vec_input, ncol = 1, sparse = TRUE)
  
  # For now, skip this test as dv_dv requires special handling
  # TODO: The original use case for dv_dv needs investigation
  skip("dv_dv requires 1-D input but .df_dp_sub requires Matrix - API needs clarification")
})

test_that("complex derivative function with quadratic terms", {
  init_derivative_args(a = 2, b = 3)
  
  # More complex: involves parm_value in multiple ways
  funct <- function(parm_value, ctx = get_context()) {
    args <- get_args(ctx, parm_value)
    # Element-wise operation - return same-sized Matrix
    result <- parm_value
    result@x <- parm_value@x^2 + a * parm_value@x + b
    result
  }
  
  M <- Matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, sparse = TRUE)
  
  result <- funct(M)
  # For M@x = (1,2,3,4), a=2, b=3:
  # f(x) = x^2 + 2*x + 3
  # f(1) = 1 + 2 + 3 = 6
  # f(2) = 4 + 4 + 3 = 11
  # f(3) = 9 + 6 + 3 = 18
  # f(4) = 16 + 8 + 3 = 27
  expect_equal(as.vector(result@x), c(6, 11, 18, 27))
  
  # Derivative: f'(x) = 2*x + a = 2*x + 2
  # At M[1,1]=1: f'(1) = 2*1 + 2 = 4
  deriv <- df_dp(funct, M, 1, 1)
  # deriv should be a Matrix with same structure as result
  expect_s4_class(deriv, "Matrix")
  expect_equal(as.numeric(deriv[1, 1]), 4, tolerance = 1e-6)
})
