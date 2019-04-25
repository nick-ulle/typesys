context("TypeEnvironment")


test_that("", {
  tenv = TypeEnvironment$new(
    "z" = TypeVariable("t3"),
    "x" = TypeVariable("t1")
  )

  sub = Substitution(list(
    "t1" = TypeVariable("t2")
  ))

  applySubstitution(tenv, sub)
})

test_that("", {
  # Related unification problem.
  type1 = FunctionType(TypeVariable("t1"), TypeVariable("t3"))
  type2 = FunctionType(TypeVariable("t2"), TypeVariable("t2"))
})
