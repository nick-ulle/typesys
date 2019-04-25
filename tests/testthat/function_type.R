context("FunctionType")

test_that("", {
  #.(n: Integer, min: Numeric, max: Numeric) ~ Integer

  FunctionType(
    n = IntegerType(),
    min = NumericType(),
    max = NumericType(),
    .return_type = IntegerType(),
    .optional = c("min", "max")
  )
})
