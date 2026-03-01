test_that(":= assigns in the calling frame", {
  local({
    A := 42
    expect_equal(A, 42)
  })
})

test_that(":= returns value invisibly", {
  local({
    ret <- (A := 10)
    expect_equal(ret, 10)
  })
})

test_that(":= sets comparison_reference via .nvd_env", {
  local({
    A := 5
    expect_equal(numVerDrvtn:::.nvd_env$comparison_reference, 5)
  })
})

test_that(":= drops near-zeroes in sparse Matrix", {
  skip_if_not_installed("Matrix")
  local({
    m <- Matrix::sparseMatrix(i = 1:2, j = 1:2, x = c(1, 1e-15), dims = c(2L, 2L))
    A := m
    expect_equal(length(A@x), 1L)  # 1e-15 dropped
  })
})

test_that(".eq returns TRUE for equal scalars", {
  expect_true(.eq(1, 1))
})

test_that(".eq returns FALSE for unequal scalars", {
  expect_false(.eq(1, 2))
})

test_that(".eq uses tolerance correctly", {
  expect_true(.eq(1, 1 + 1e-10, tol = 1e-9))
  expect_false(.eq(1, 1 + 1e-8,  tol = 1e-9))
})

test_that(".eq.last compares against comparison_reference", {
  local({
    A := 7
    result <- .eq.last(7)
    expect_true(result)
  })
})

test_that(".eq.last returns FALSE for mismatch", {
  local({
    A := 7
    result <- .eq.last(8)
    expect_false(result)
  })
})

test_that(".eq.last errors when no reference set", {
  # Reset comparison_reference
  old_ref <- numVerDrvtn:::.nvd_env$comparison_reference
  numVerDrvtn:::.nvd_env$comparison_reference <- NULL
  on.exit(numVerDrvtn:::.nvd_env$comparison_reference <- old_ref)

  expect_error(.eq.last(1), "comparison_reference is NULL")
})

test_that(". == expr calls .eq.last", {
  local({
    A := 3
    result <- . == 3
    expect_true(result)
  })
})

test_that(".asgn assigns and sets reference", {
  local({
    .asgn(B, 99)
    expect_equal(B, 99)
    expect_equal(numVerDrvtn:::.nvd_env$comparison_reference, 99)
  })
})

test_that("definedExpression.options sets print_differences", {
  old <- definedExpression.options(print_differences = FALSE)
  expect_false(numVerDrvtn:::.nvd_env$print_differences)
  definedExpression.options(print_differences = TRUE)
  expect_true(numVerDrvtn:::.nvd_env$print_differences)
  # restore
  definedExpression.options(print_differences = old)
})
