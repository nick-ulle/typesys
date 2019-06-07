#' Term
#'
#' Superclass for all syntactic terms typesys can manipulate.
#'
#' @name Term-class
#' @exportClass typesys::Term
setClass("typesys::Term", contains = "VIRTUAL")


#' @exportClass typesys::Variable
setClass("typesys::Variable", contains = "typesys::Term",
  slots = list(
    name = "character"
  ))
setValidity("typesys::Variable", function(object) {
  if (length(object@name) != 1)
    "name must be a length 1 character vector"
  else
    TRUE
})

#' @export
Variable = function(name) new("typesys::Variable", name = name)


#' @exportClass typesys::Constant
setClass("typesys::Constant", contains = c("typesys::Term", "VIRTUAL"))



# Composites ----------------------------------------

#' @exportClass typesys::Composite
setClass("typesys::Composite", contains = c("typesys::Term", "VIRTUAL"),
  slots = list(
    components = "list"
  ))
setValidity("typesys::Composite", function(object) {
  if ( all(vapply(object@components, is, NA,"typesys::Term")) )
    TRUE
  else
    "components must subclass typesys::Term"
})

#' @exportClass typesys::Function
setClass("typesys::Function", contains = "typesys::Composite")
setValidity("typesys::Function", function(object) {
  if (length(object@components) > 0L)
    TRUE
  else
    "must have return type (at least 1 component)"
})

Function = function(..., return_type) {
  components = list(...)

  # Flatten first element if a list.
  if (length(components) > 0L && is.list(components[[1L]])) {
    components = c(components[[1L]], components[-1L])
  }

  # Check return_type parameter.
  if (!missing(return_type)) {
    len = length(components) + 1L
    components[[len]] = return_type
  }

  new("typesys::Function", components = components)
}


# Applications are ?<?>, where both the type applied and the type arguments are
# free. Applications are useful if we want to reason about the type applied,
# which is not easy to do with the less flexible parametric type T<?>.
#
# For instance, what if we have code:
#
#   function(x) {
#     x[1] = 42L
#     x
#   }
#
# At the first line, we know x has elements but can't tell whether x is a list,
# a vector, or some other type of object. So how should the type system
# represent this information, or what should happen if someone tries to run
# type inference on this code?
#
# Let's start with only Variables, Constants, and Composites.
