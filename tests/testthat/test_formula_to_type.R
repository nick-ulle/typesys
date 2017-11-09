context("formula_to_type")


test_that("type variables", {
  name = "x"

  result = expr_to_type(as.name(name))

  # -----
  expect_is(result, "typesys::TypeVar")
  expect_equal(result@name, name)
})


test_that("type constants", {
  result1 = expr_to_type(as.name("Boolean"))
  result2 = expr_to_type(as.name("Integer"))
  result3 = expr_to_type(as.name("Real"))
  result4 = expr_to_type(as.name("Complex"))

  # -----
  expect_is(result1, "typesys::BooleanType")
  expect_is(result2, "typesys::IntegerType")
  expect_is(result3, "typesys::RealType")
  expect_is(result4, "typesys::ComplexType")
})


test_that("functions", {
  result = formula_to_type(c(a, Boolean, b) ~ Integer)

  # -----
  expect_is(result, "typesys::FunctionType")
  args = result@args
  expect_equal(length(args), 3)
  expect_is(args[[1]], "typesys::TypeVar")
  expect_is(args[[2]], "typesys::BooleanType")
  expect_is(args[[3]], "typesys::TypeVar")
  expect_is(result@return_type, "typesys::IntegerType")
})
