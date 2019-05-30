context("formula_to_type")


test_that("type variables", {
  name = "x"

  result = formula_to_type(as.name(name))

  # -----
  expect_is(result, "typesys::Variable")
  expect_equal(result@name, name)
})


test_that("type constants", {
  result1 = formula_to_type(as.name("RLogical"))
  result2 = formula_to_type(as.name("RInteger"))
  result3 = formula_to_type(as.name("RNumeric"))
  result4 = formula_to_type(as.name("RComplex"))

  # -----
  expect_is(result1, "typesys::RLogical")
  expect_is(result2, "typesys::RInteger")
  expect_is(result3, "typesys::RNumeric")
  expect_is(result4, "typesys::RComplex")
})


test_that("functions", {
  result = formula_to_type(c(a, RLogical, b) ~ RInteger)

  # -----
  expect_is(result, "typesys::Function")
  args = args(result)
  expect_equal(length(args), 3)
  expect_is(args[[1]], "typesys::Variable")
  expect_is(args[[2]], "typesys::RLogical")
  expect_is(args[[3]], "typesys::Variable")
  expect_is(return_type(result), "typesys::RInteger")
})


test_that("Types", {
  type = RInteger
  result = formula_to_type(type)

  # -----
  expect_identical(type, result)
})
