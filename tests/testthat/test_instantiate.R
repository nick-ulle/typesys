context("instantiate")

test_that("constant instantiation", {
  skip_if_not_installed("rstatic")

  term = RInteger
  result = instantiate(term)

  # -----
  expect_identical(term, result)
})

test_that("variable instantiation", {
  skip_if_not_installed("rstatic")

  term = Variable("x")
  result1 = instantiate(term)

  result2 = instantiate(term, list(Variable("x")))
  # -----
  expect_is(result1, "typesys::Variable")
  expect_false(identical(term, result1))

  expect_identical(term, result2)
})

test_that("implicit instance instantiation", {
  skip_if_not_installed("rstatic")

  con = ImplicitInstance(
    Variable("a") , Function(Variable("b"), Variable("c"))
    # Monomorphic set
    , list(Variable("b")) )

  result = instantiate(con)

  # -----
  expect_is(result, "typesys::ImplicitInstance")

  # Check that 'b' was not changed.
  t2 = con@t2
  new_t2 = result@t2
  expect_identical(args(new_t2)[[1L]], args(t2)[[1L]])

  # Check that 'c' was changed.
  expect_false(identical(return_type(new_t2), return_type(t2)))
})
