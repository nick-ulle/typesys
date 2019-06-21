# Getters and setters for Term objects.

#' @export
setGeneric("args")

#' @export
setMethod("args", signature("typesys::Function"),
  function(name) {
    len = length(name@components)
    name@components[-len]
  })


# ----------------------------------------
#' @export
setGeneric("return_type", function(fn) standardGeneric("return_type"),
  valueClass = "typesys::Term")

#' @export
setMethod("return_type", signature("typesys::Function"),
  function(fn) {
    len = length(fn@components)
    fn@components[[len]]
  })



#' Get Variables
#'
#' This function returns a list of all variables in a term or constrain.
#'
#' In an ImplicitInstance constraint, some variables in the right-hand term may
#' be generic rather than monomorphic. Generic variables are replaced with new
#' variables when a constraint is solved, whereas monomorphic variables are
#' not. The `generic` parameter controls whether generic variable are included
#' in returned the list of variables.
#'
#' @param term (Term|Constraint) The term to search for variables.
#' @param generic (logical) Should generic variables be included in the list?
#'
#' @return A list of Variable objects.
#'
#' @export
setGeneric("vars",
function(term, generic = FALSE)
  standardGeneric("vars")
)


#' @export
setMethod("vars", signature("typesys::Variable"),
function(term, generic = FALSE)
{
  list(term)
})


#' @export
setMethod("vars", signature("typesys::Constant"),
function(term, generic = FALSE)
{
  list()
})


#' @export
setMethod("vars", signature("typesys::Composite"),
function(term, generic = FALSE)
{
  result = lapply(term@components, vars, generic = generic)
  result = unlist(result, recursive = FALSE, use.names = FALSE)
  # Make sure an empty list is returned instead of NULL.
  as.list(result)
})


#' @export
setMethod("vars", signature("typesys::Constraint"),
function(term, generic = FALSE)
{
  v1 = vars(term@t1, generic)
  v2 = vars(term@t2, generic)
  union(v1, v2)
})


setMethod("vars", signature("typesys::ImplicitInstance"),
function(term, generic = FALSE)
{
  if (generic)
    return (callNextMethod())

  v1 = vars(term@t1, generic)
  v2 = intersect(vars(term@t2, generic), term@monomorphic)
  union(v1, v2)
})
