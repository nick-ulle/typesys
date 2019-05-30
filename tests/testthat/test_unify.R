context("unify")


test_that("Variable and constant", {
  x = Variable("a")
  y = RInteger

  result1 = unify(x, y)
  result2 = unify(y, x)

  # -----
  expect_is(result1, "typesys::Substitution")
  expect_equal(length(result1), 1)
  expect_is(result1[["a"]], "typesys::RInteger")

  expect_is(result2, "typesys::Substitution")
  expect_equal(length(result2), 1)
  expect_is(result2[["a"]], "typesys::RInteger")
})


test_that("Variable and function", {
  x = formula_to_type(a ~ RInteger)
  y = Variable("b")

  result1 = unify(x, y)
  result2 = unify(y, x)

  # -----
  expect_equal(length(result1), 1)
  expect_is(result1[["b"]], "typesys::Function")

  expect_equal(length(result2), 1)
  expect_is(result2[["b"]], "typesys::Function")
})


test_that("Function and function", {
  x = formula_to_type(a ~ RInteger)
  y = formula_to_type(RNumeric ~ RInteger)

  result1 = unify(x, y)
  result2 = unify(y, x)

  # -----
  expect_equal(length(result1), 1)
  expect_is(result1[["a"]], "typesys::RNumeric")

  expect_equal(length(result2), 1)
  expect_is(result2[["a"]], "typesys::RNumeric")
})


test_that("Incompatible types", {
  x = RInteger
  y = RNumeric
  z = formula_to_type(a ~ b)

  # -----
  expect_error(unify(x, y))
  expect_error(unify(y, x))
  expect_error(unify(x, z))
})
