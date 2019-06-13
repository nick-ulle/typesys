context("unify")


test_that("Variable and Constant", {
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


test_that("Variable and Function", {
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


test_that("Function and Function", {
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


test_that("ImplicitInstance", {
  f1 = RFunction(RNumeric, Variable("a"))
  f2 = RFunction(Variable("b"), Variable("b"))
  con = ImplicitInstance(f1, f2)
  sub = Substitution(x = Variable("b"))

  result = unify(con, sub)

  # -----
  expect_is(result[["a"]], "typesys::RNumeric")

  # Check that b was instantiated by checking that no type was found for b.
  expect_false("b" %in% names(result))
})

test_that("Equivalence", {
  eq = Equivalence(RInteger, Variable("a"))
  result = unify(eq)

  expect_is(result, "typesys::Substitution")
  expect_equal(length(result), 1)
  expect_is(result[["a"]], "typesys::RInteger")

  eq2 = Equivalence(RInteger, RNumeric)
  expect_error(unify(eq2))
})
