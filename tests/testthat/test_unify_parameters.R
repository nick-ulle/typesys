context("unify parameters")


test_that("", {
  p1 = ParameterType(x = IntegerType())
  p2 = ParameterType(x = IntegerType())

  result = unify(p1, p2)
})


test_that("", {
  p1 = ParameterType(IntegerType())
  p2 = ParameterType(x = TypeVariable("a"))

  result = unify(p1, p2)
})


test_that("", {
  p1 = ParameterType(IntegerType(), x = TypeVariable("a"))
  p2 = ParameterType(x = NumericType())

  unify(p1, p2)
})
