test_that("DefinedExpression is a virtual S4 class", {
  expect_true(isVirtualClass("DefinedExpression"))
})

test_that("dotSentinel is a sub-class of character", {
  expect_true(is(., "character"))
  expect_true(is(., "dotSentinel"))
})

test_that("is.DefinedExpression returns FALSE for plain objects", {
  expect_false(is.DefinedExpression(1))
  expect_false(is.DefinedExpression("hello"))
  expect_false(is.DefinedExpression(matrix(1:4, 2, 2)))
})

test_that("a class that inherits DefinedExpression passes is.DefinedExpression", {
  setClass("TestDE", contains = "DefinedExpression")
  obj <- methods::new("TestDE")
  expect_true(is.DefinedExpression(obj))
})
