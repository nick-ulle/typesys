context("Union")


test_that("Union of same types returns type", {
  result = Union(IntegerType(), IntegerType())

  # -----
  expect_is(result, "typesys::IntegerType")
})


test_that("Union of different types returns Union", {
  result = Union(IntegerType(), ComplexType())

  # -----
  expect_is(result, "typesys::Union")
  expect_is(result[[1]], "typesys::IntegerType")
  expect_is(result[[2]], "typesys::ComplexType")
})


test_that("Union of Unions is flattened", {
  result = Union(Union("x", RealType()), Union(RealType(), "y"))

  # ----
  expect_is(result, "typesys::Union")
  expect_equal(result[[1]], "x")
  expect_is(result[[2]], "typesys::RealType")
  expect_equal(result[[3]], "y")
})
