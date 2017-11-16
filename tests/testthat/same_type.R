# Description:
#   Tests for the `sameType` method.

test_that("same_type detects matching atomic types", {
  x = RealType()
  y = RealType()

  expect_true(same_type(x, y))
})

test_that("same_type detects different atomic types", {
  x = RealType()
  y = IntegerType()

  expect_false(same_type(x, y))
})

test_that("same_type detects matching composite types", {
})

test_that("same_type detects different composite types", {
})
