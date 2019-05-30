context("compare")

test_that("== operator", {
  # Variables
  expect_true(Variable("x") == Variable("x"))
  expect_false(Variable("x") == Variable("y"))

  # Functions
  expect_true(
    Function(Variable("x"), Variable("y"), ret = Variable("z")) ==
    Function(Variable("x"), Variable("y"), ret = Variable("z"))
  )
  expect_false(
    Function(Variable("x"), Variable("x"), ret = Variable("z")) ==
    Function(Variable("x"), Variable("y"), ret = Variable("z"))
  )
  expect_false(
    Function(Variable("x"), Variable("y"), ret = Variable("x")) ==
    Function(Variable("x"), Variable("y"), ret = Variable("z"))
  )
})

test_that("!= operator", {
  # Variables
  expect_true(Variable("ab") != Variable("ba"))
  expect_false(Variable("ab") != Variable("ab"))

  # Other
  expect_true(Variable("a") != 3)
  expect_true(
    Variable("A") != Function(Variable("A"), ret = Variable("A"))
  )
})
