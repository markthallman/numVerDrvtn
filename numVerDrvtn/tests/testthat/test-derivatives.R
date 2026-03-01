test_that("new_derivative_context creates a DerivativeContext", {
  ctx <- new_derivative_context(list(x = 1, y = 2))
  expect_s3_class(ctx, "DerivativeContext")
  expect_equal(ctx$derivative_args$x, 1)
  expect_equal(ctx$derivative_args$y, 2)
})

test_that("init_derivative_args errors on unnamed arguments", {
  expect_error(init_derivative_args(1, 2), "must be named")
})

test_that("init_derivative_args stores context", {
  init_derivative_args(a = 10, b = 20)
  ctx <- get_context()
  expect_s3_class(ctx, "DerivativeContext")
  expect_equal(ctx$derivative_args$a, 10)
  expect_equal(ctx$derivative_args$b, 20)
})

test_that("get_context errors when no context set", {
  old <- numVerDrvtn:::.nvd_env$last_deriv_ctx
  numVerDrvtn:::.nvd_env$last_deriv_ctx <- NULL
  on.exit(numVerDrvtn:::.nvd_env$last_deriv_ctx <- old)
  expect_error(get_context(), "no derivative context")
})

test_that("df_dp computes correct numerical derivative for simple scalar function", {
  skip_if_not_installed("Matrix")
  skip_if_not_installed("numDeriv")
  skip_if_not_installed("digest")

  # f(A) = A %*% v_fixed; df/dA[1,1] should be v_fixed[1]
  v_fixed <- c(2, 3)
  A_mat   <- Matrix::Matrix(c(1, 0, 0, 1), nrow = 2L, ncol = 2L, sparse = TRUE)
  A_mat   <- methods::as(A_mat, "dgCMatrix")

  init_derivative_args(v = v_fixed)

  funct <- function(A, ctx = get_context()) {
    args <- get_args(ctx, A)
    A %*% v
  }

  deriv_11 <- df_dp(funct, A_mat, row = 1L, col = 1L)
  # df/dA[1,1] = v[1] = 2; the result is the first row of the Jacobian
  expect_equal(as.numeric(deriv_11)[1L], 2, tolerance = 1e-6)
})
