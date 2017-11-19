context("quantify")


test_that("Literals are unaffected", {
  texp = IntegerType()

  result = quantify(texp)

  # -----
  expect_identical(texp, result)
})


test_that("Variables are quantified", {
  texp = TypeVariable("a")

  result = quantify(texp)

  # -----
  expect_is(result, "typesys::TypeVariable")
  expect_equal(result@quantified, "a")
})


test_that("Variables in functions are quantified", {
  texp = FunctionType(list(TypeVariable("a"), TypeVariable("b")),
    TypeVariable("a"))

  result = quantify(texp)

  # -----
  expect_is(result, "typesys::FunctionType")
  expect_equal(sort(result@quantified), c("a", "b"))
})
