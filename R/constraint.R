
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

#' @exportClass typesys::Equality
setClass("typesys::Equality", contains = "typesys::Constraint")

Equality = function(t1, t2) {
  new("typesys::Equality", t1 = t1, t2 = t2)
}

#' @exportClass typesys::ImplicitInstance
setClass("typesys::ImplicitInstance", contains = "typesys::Constraint")

ImplicitInstance = function(t1, t2, m) {
  new("typesys::ImplicitInstance", t1 = t1, t2 = t2)
}

## #' @exportClass typesys::ExplicitInstance
## setClass("typesys::ExplicitInstance", contains = "typesys::Constraint")
