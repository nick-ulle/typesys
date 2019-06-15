
#' Solve a List of Constraints
#'
#' This function solves a list of constraints.
#'
#' @param constraints List of Constraint objects to solve.
#' @param counter Counter for generating new variables.
#' @return The solution to the supplied constraints as a Substitution object.
#'
#' @references
#' Heeren, Hage, and Swierstra (2002). Generalizing Hindley-Milner Type
#' Inference Algorithms.
#'
#' @export
solve = function(constraints, counter) {
  # First solve the equivalence constraints.
  is_eq = vapply(constraints, is, NA, "typesys::Equivalence")
  constraints = split(constraints, is_eq)

  sub = typesys::Substitution()
  for (con in constraints[["TRUE"]]) {
    update = typesys::unify(con, sub = sub)
    sub = update(sub)
  }

  # Now solve the instance constraints, which may need to be solved in a
  # specific order.
  constraints = constraints[["FALSE"]]
  constraints = lapply(constraints, sub)

  while (length(constraints) > 0L) {
    # Pop off a constraint.
    con = constraints[[1L]]
    constraints = constraints[-1L]

    # FIXME: Check whether the constraint can be solved.
    # If (freevars(t2) - M) ^ activevars(C) is empty

    # Solve the constraint.
    sub = typesys::unify(con, sub = sub, counter = counter)(sub)
  }

  sub
}
