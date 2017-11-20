context("unify")


test_that("Variable and constant", {
  x = TypeVariable("a")
  y = IntegerType()

  result1 = unify(x, y)
  result2 = unify(y, x)

  # -----
  expect_is(result1, "Substitution")
  expect_equal(length(result1), 1)
  expect_is(result1[["a"]], "typesys::IntegerType")

  expect_is(result2, "Substitution")
  expect_equal(length(result2), 1)
  expect_is(result2[["a"]], "typesys::IntegerType")
})


test_that("Variable and function", {
  x = formula_to_type(a ~ Integer)
  y = TypeVariable("b")

  result1 = unify(x, y)
  result2 = unify(y, x)

  # -----
  expect_equal(length(result1), 1)
  expect_is(result1[["b"]], "typesys::FunctionType")

  expect_equal(length(result2), 1)
  expect_is(result2[["b"]], "typesys::FunctionType")
})


test_that("Function and function", {
  x = formula_to_type(a ~ Integer)
  y = formula_to_type(Numeric ~ Integer)

  result1 = unify(x, y)
  result2 = unify(y, x)

  # -----
  expect_equal(length(result1), 1)
  expect_is(result1[["a"]], "typesys::NumericType")

  expect_equal(length(result2), 1)
  expect_is(result2[["a"]], "typesys::NumericType")
})


test_that("Incompatible types", {
  x = IntegerType()
  y = NumericType()
  z = formula_to_type(a ~ b)

  # -----
  expect_error(unify(x, y))
  expect_error(unify(y, x))
  expect_error(unify(x, z))
})
