test_that("definedExpression is a virtual S4 class", {
  expect_true(isVirtualClass("definedExpression"))
})

test_that("dotSentinel is a sub-class of character", {
  expect_true(is(., "character"))
  expect_true(is(., "dotSentinel"))
})

test_that("is.definedExpression returns FALSE for plain objects", {
  expect_false(is.definedExpression(1))
  expect_false(is.definedExpression("hello"))
  expect_false(is.definedExpression(matrix(1:4, 2, 2)))
})

test_that("a class that inherits definedExpression passes is.definedExpression", {
  setClass("TestDE", contains = "definedExpression")
  obj <- methods::new("TestDE")
  expect_true(is.definedExpression(obj))
})
