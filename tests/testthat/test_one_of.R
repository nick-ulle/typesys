context("OneOf")


test_that("simplify", {
  result = OneOf(RNumeric, RNumeric)

  # -----
  expect_identical(result, RNumeric)
})


test_that("types and variables", {
  result = OneOf(Variable("RNumeric"), RNumeric)

  # -----
  expect_length(result@components, 2L)
  expect_true(Variable("RNumeric") %in% result)
  expect_true(RNumeric %in% result)
})


test_that("duplicate variables removed", {
  result = OneOf(Variable("x"), Variable("y"), Variable("x"))

  # -----
  expect_length(result@components, 2L)
  expect_true(Variable("x") %in% result)
  expect_true(Variable("y") %in% result)
})


test_that("duplicate types removed", {
  result = OneOf(RNumeric, RInteger, RInteger)

  # -----
  expect_length(result@components, 2L)
  expect_true(RNumeric %in% result)
  expect_true(RInteger %in% result)
})
