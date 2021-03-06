#' Solve a List of Constraints
#'
#' This function solves a list of constraints.
#'
#' @param x An object that contains a list of constraints to solve.
#' @param ... Additional arguments to methods.
#'
#' @return The solution to the supplied constraints as a Substitution object.
#'
#' @references
#' Heeren, Hage, and Swierstra (2002). Generalizing Hindley-Milner Type
#' Inference Algorithms.
#'
#' @export
setGeneric("csolve",
function(x, ...)
  standardGeneric("csolve")
)


#' @param counter (Counter) A Counter for generating new variables.
#' @param max_deferred (integer) Maximum number of times the solver can defer
#' constraints before concluding the system has no solutions.
#'
#' @describeIn csolve Solve a list of constraints.
#'
#' @export
setMethod("csolve", "list",
function(x, counter, max_deferred = 1e3L, ...)
{
  sub = Substitution()
  is_equivalence = vapply(x, is, NA, "typesys::Equivalence")

  # First solve the equivalence constraints.
  for (con in x[is_equivalence]) {
    update = unify(con, sub = sub)
    sub = update(sub)
  }

  # Now solve the instance constraints, which may need to be solved in a
  # specific order.
  constraints = x[!is_equivalence]
  constraints = lapply(constraints, sub)

  deferred = 0L
  while (length(constraints) > 0L) {
    # Pop off a constraint.
    con = constraints[[1L]]
    constraints = constraints[-1L]

    # Check that the generic variables are not active.
    # NOTE: This assumes the constraints are all ImplicitInstance constraints.
    generics = setdiff(vars(con@t2), con@monomorphic)
    active = lapply(constraints, vars)
    active = unlist(active, recursive = FALSE, use.names = FALSE)

    if (any(generics %in% active)) {
      deferred = deferred + 1L
      # Check that we haven't deferred too many times, to avoid getting stuck
      # in an infinite loop.
      if (deferred >= max_deferred)
        stop(sprintf("Solver deferred constraints %i times. Check that the
            constraint set is consistent or increase 'max_defers'.", deferred))

      constraints = c(constraints, con)
      next
    }

    # Solve the constraint.
    sub = unify(con, sub = sub, counter = counter)(sub)
  }

  sub
})
