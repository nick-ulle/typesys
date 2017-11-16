context("join")


test_that("Join on 1 type is just that type", {
  join1 = Join(IntegerType())
  join2 = Join(TypeVar("a"))

  # -----
  expect_is(join1, "typesys::IntegerType")
  expect_is(join2, "typesys::TypeVar")
})


test_that("Redundant types are removed", {
  join1 = Join(BooleanType(), TypeVar("a"), BooleanType())
  join2 = Join(TypeVar("a"), TypeVar("b"), TypeVar("a"))

  # -----
  expect_equal(length(join1@args), 2)

  expect_equal(length(join2@args), 2)
  vars = vapply(join2@args, slot, NA_character_, "name")
  vars = sort(vars)
  expect_equal(vars, c("a", "b"))
})


test_that("Nested joins are flattened", {
  join = Join(IntegerType(), RealType())
  join@args[[1]] = Join(IntegerType(), RealType())

  join = simplify(join)

  # -----
  expect_equal(length(join@args), 2)

  classes = vapply(join@args, function(arg) class(arg)[1], NA_character_)
  classes = sort(classes)
  expect_equal(classes, c("typesys::IntegerType", "typesys::RealType"))
})
