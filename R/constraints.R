
#' Constraint
#'
#' Superclass for all constraints on terms.
#'
#' Constraints are pairs of terms with a relation between them. Constraint
#' subclasses represent specific relations.
#'
#' @name Constraint-class
#' @exportClass typesys::Constraint
setClass("typesys::Constraint", contains = "VIRTUAL",
  slots = list(
    t1 = "typesys::Term",
    t2 = "typesys::Term"
  ))

#' @exportClass typesys::Equivalence
setClass("typesys::Equivalence", contains = "typesys::Constraint")

#' @export
Equivalence = function(t1, t2) {
  new("typesys::Equivalence", t1 = t1, t2 = t2)
}

#' @exportClass typesys::ImplicitInstance
setClass("typesys::ImplicitInstance", contains = "typesys::Constraint")

#' @export
ImplicitInstance = function(t1, t2, m) {
  new("typesys::ImplicitInstance", t1 = t1, t2 = t2)
}

## #' @exportClass typesys::ExplicitInstance
## setClass("typesys::ExplicitInstance", contains = "typesys::Constraint")
