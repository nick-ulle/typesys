#' @include terms.R
NULL

#' @export
setClass("typesys::Substitution", contains = "function",
  slots = list(
    map = "list"
  ))
setValidity("typesys::Substitution", function(object) {
  map = object@map
  if (length(map) == 0)
    return (TRUE)

  names = names(map)
  if ( is.null(names) || any(duplicated(names)) )
    return("elements must have unique names")

  if ( all(vapply(map, is, NA, "typesys::Term")) )
    TRUE
  else
    sprintf("elements must be Term objects")
  })

#' @export
Substitution = function(...) {
  sub = list(...)
  len = length(sub)

  # Allow passing a list.
  if ( (len == 1) && is.list(sub[[1]]) ) {
    sub = sub[[1]]
  }

  new("typesys::Substitution", map = sub,
    # Here sys.function() gets this S4 object.
    function(term) do_substitution(term, sys.function())
  )
}

# TODO: Consider making this part of the Substitution() constructor.
make_substitution = function(var, term) {
  sub = structure(list(term), names = names(var))
  Substitution(sub)
}


#' Apply a Substitution
#'
#' This generic function applies a Substitution to a Term or to another
#' Substitution (composition).
#'
#' Substitutions are callable, and this is the generic they call. For clarity,
#' calling a Substitution directly on a term is preferable to calling this
#' function.
#' 
#' @param term A Term or Substitution object.
#' @param sub A Substitution object to apply.
#' @return An object of the same class as \code{term}.
#'
#' @export
setGeneric("do_substitution",
  function(term, sub) standardGeneric("do_substitution")
)

#' @export
setMethod("do_substitution", signature("typesys::Variable"),
function(term, sub) {
  ans = sub[[term]]
  if (is.null(ans))
    term
  else
    ans
})

#' @export
setMethod("do_substitution", signature("typesys::Constant"),
  function(term, sub) term)

#' @export
setMethod("do_substitution", signature("typesys::Composite"),
  function(term, sub) {
    term@components = lapply(term@components, sub)
    term
  })

#' @export
setMethod("do_substitution", signature("typesys::Substitution"),
  function(term, sub) {
    # Term is also a substitution, so this is composition.
    # Apply substitution to each element.
    term@map = lapply(term@map, sub)

    # Add entries not already present.
    new_entries = setdiff(names(sub), names(term))
    term[new_entries] = sub[new_entries]

    term
  })
