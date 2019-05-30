context("substitution")


test_that("Subtitution elements must have unique names", {
  expect_error( Substitution(list(x = 3, x = 4)) )
})


test_that("Substitution cannot have non-Term elements", {
  expect_error(Substitution(x = 3))
  expect_error(Substitution(x = Variable("y"), y = 3))
})


test_that("Subtitution [[ works with integers, strings, and Variables", {
  sub = Substitution(x = Variable("y"), a = Variable("b"))

  result1 = sub[["x"]]
  expect_is(result1, "typesys::Variable")
  expect_equal(result1@name, "y")

  result2 = sub[[2]]
  expect_is(result2, "typesys::Variable")
  expect_equal(result2@name, "b")

  result3 = sub[[Variable("x")]]
  expect_is(result3, "typesys::Variable")
  expect_equal(result3@name, "y")
})


test_that("Substitution called on Variable substitutes", {
  sub = Substitution(x = Variable("y"))
  result = sub(Variable("x"))

  expect_is(result, "typesys::Variable")
  expect_equal(result@name, "y")
})


test_that("Substitution called on Constant has no effect", {
  sub = Substitution(x = Variable("y"))
  result = sub(RComplex)

  expect_is(result, "typesys::Constant")
})

test_that("Substitution called on Function substitutes", {
  sub = Substitution(x = Variable("y"))
  fun = Function(Variable("y"), Variable("x"))
  result = sub(fun)

  expect_is(result, "typesys::Function")

  arg = args(result)[[1L]]
  expect_is(arg, "typesys::Variable")
  expect_equal(arg@name, "y")

  ret = return_type(result)
  expect_is(ret, "typesys::Variable")
  expect_equal(ret@name, "y")
})

test_that("Substitution called on Substitution composes", {
  sub1 = Substitution(a = Variable("b"))
  sub2 = Substitution(b = Variable("c"), x = Variable("y"))

  result = sub2(sub1)
  expect_is(result, "typesys::Substitution")
  a = result[["a"]]
  expect_is(a, "typesys::Variable")
  expect_equal(a@name, "c")
  x = result[["x"]]
  expect_is(x, "typesys::Variable")
  expect_equal(x@name, "y")
})
