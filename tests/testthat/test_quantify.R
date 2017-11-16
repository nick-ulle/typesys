context("quantify")


test_that("Literals are unaffected", {
  texp = IntegerType()

  result = quantify(texp)

  # -----
  expect_identical(texp, result)
})


test_that("Variables are quantified", {
  texp = TypeVar("a")

  result = quantify(texp)

  # -----
  expect_is(result, "typesys::TypeVar")
  expect_equal(result@quantified, "a")
})


test_that("Variables in functions are quantified", {
  texp = FunctionType(list(TypeVar("a"), TypeVar("b")), TypeVar("a"))

  result = quantify(texp)

  # -----
  expect_is(result, "typesys::FunctionType")
  expect_equal(sort(result@quantified), c("a", "b"))
})
