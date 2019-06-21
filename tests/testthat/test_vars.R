context("vars")


test_that("Instance constraint contains all variables in left term, and all
  variables in right term that are monomorphic",
{
  t1 = Variable("x")

  subterm2.1 = Variable("monomorphic")
  subterm2.2 = Variable("generic")
  t2 = Function(subterm2.1, subterm2.2, RNumeric, RInteger)

  result = vars(ImplicitInstance(t1, t2, list(subterm2.1)))

  # -----
  expect_length(result, 2L)
  expect_identical(result[[1L]], t1)
  expect_identical(result[[2L]], subterm2.1)
})


test_that("Equivalence constraint contains variables in both terms", {
  t1 = Variable("x")

  subterm2.1 = Variable("fork")
  subterm2.2 = Variable("spoon")
  t2 = Function(RNumeric, subterm2.1, subterm2.2)

  result = vars(Equivalence(t1, t2))

  # -----
  expect_length(result, 3L)
  expect_identical(result[[1L]], t1)
  expect_identical(result[[2L]], subterm2.1)
  expect_identical(result[[3L]], subterm2.2)
})


test_that("Function contains variables in all subterms", {
  subterms = list(
    Variable("cats")
    , Variable("are")
    , Variable("fuzzy")
  )

  term = Function(subterms, RNumeric)

  result = vars(term)

  # -----
  expect_length(result, 3L)
  for (i in seq_along(subterms))
    expect_identical(result[[i]], subterms[[i]])
})


test_that("Variable contains itself", {
  term = Variable("x")

  result = vars(term)

  # -----
  expect_length(result, 1L)
  expect_identical(result[[1L]], term)
})


test_that("Constant contains no variables", {
  term = RInteger

  result = vars(term)

  # -----
  expect_length(result, 0L)
})
