context("solve")


test_that("Inconsistent constraint set raises error", {
  x = Variable("x")
  y = Variable("y")
  cons = list(
    ImplicitInstance(x, y)
    , ImplicitInstance(y, x)
  )

  expect_error(solve(cons, rstatic::Counter$new(), 100L))
})


test_that("Variables are polymorphic", {
  skip_if_not_installed("RTypeInference", "0.5.0")
  skip_if_not_installed("rstatic")

  node = rstatic::quote_ast({
    f = function(x) {
      x
    }

    f(3)
    f("hi")
  })

  # Create a counter that will generate unique names for type variables.
  counter = rstatic::Counter$new()

  # Compute type constraints for the code above.
  #
  # The result is the list of constraints and also an InferHelper object that
  # records how code variables map to type variables.
  #
  result1 = RTypeInference::constrain(node, counter = counter)
  cons = result1[["constraints"]]

  # Solve the type constraints. The result is a substitution that maps type
  # variables to types.
  #
  # I haven't written a function to map the solutions back to code variables
  # yet, but I'll add that to RTypeInference soon. 
  #
  result2 = solve(cons, counter)

  # -----
  #browser()
})


test_that("Variables from parameters are monomorphic", {
  skip_if_not_installed("RTypeInference", "0.5.0")
  skip_if_not_installed("rstatic")

  node = rstatic::quote_ast({
    f = function(g) {
      h = g
      h(3)
      h("hi")
    }
  })

  counter = rstatic::Counter$new()
  cons = RTypeInference::constrain(node, counter = counter)[["constraints"]]

  # -----
  expect_error(solve_types(cons, counter))
})


test_that("Parameters are monomorphic", {
  skip_if_not_installed("RTypeInference", "0.5.0")
  skip_if_not_installed("rstatic")

  node = rstatic::quote_ast({
    f = function(g) {
      g(3)
      g("hi")
    }
  })

  counter = rstatic::Counter$new()
  cons = RTypeInference::constrain(node, counter = counter)[["constraints"]]

  # -----
  expect_error(solve_types(cons, counter))
})
