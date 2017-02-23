# Description:
#
# Definitions for types that aren't really types, but are useful for inference
# or other applications.


#' Union
#'
#' A union of two or more types.
#'
#' @export
Union = function(...) {
  u = new("Union", types = list(...))
  simplify(u)
}

#' @slot types A list of types.
#' @rdname Union
#' @exportClass Union
setClass("Union", contains = "Type",
  slots = list(
    types = "list"
  )

  #validity = function(object) {
  #  messages = character(0)

  #  is_type = sapply(object@types, is, "Type")
  #  if (!all(is_type))
  #    messages = c(messages, "types must have superclass Type.")

  #  if (length(messages) > 0) messages
  #  else TRUE
  #}
)

#' @export
setMethod("simplify", signature(x = "Union"),
  function(x) {
    # FIXME: Remove extraneous types.
    types = x@types
    same_type = vapply(types[-1], identical, logical(1), types[[1]])

    if (all(same_type))
      simplified = types[[1]]
    else
      simplified = x

    return (simplified)
  }
)

#' @export
setMethod("[[", signature(x = "Union"),
  function(x, i, ...) {
    return (x@types[[i]])
  }
)

#' @export
setMethod("length", signature(x = "Union"),
  function(x) {
    return (length(x@types))
  }
)

#' @export
setMethod("format", signature(x = "Union"),
  function(x, indent = 0, ...) {
    args = vapply(x@types, format, character(1))
    sprintf("Union(%s)", paste(args, collapse = ", "))
  }
)

#' Call
#'
#' A call to a function.
#'
#' @export
Call = function(func, ...) {
  new("Call", func = func, args = list(...))
}

#' @slot func The name of the function to call
#' @slot args A list of argument types
#' @rdname Call
#' @exportClass Call
setClass("Call", contains = "Type",
  slots = list(
    func = "character",
    args = "list"
  )
)

#' @export
setMethod("format", signature(x = "Call"),
  function(x, indent = 0, ...) {
    args = vapply(x@args, format, character(1))
    sprintf("%s(%s)", x@func, paste(args, collapse = ", "))
  }
)

#' Unknown Type
#'
#' "Top" type in type lattice, which indicates a variable whose type is
#' unknown.
#'
#' @export
UnknownType = function() new("UnknownType")

#' @rdname UnknownType
#' @exportClass UnknownType
setClass("UnknownType", contains = "Type")


# TODO: Incorporate conditional typing.
#' Undefined Type
#'
#' "Bottom" type in the type lattice, which indicates a variable which is
#' nonexistent or has not yet been initialized.
#'
#' @export
UndefinedType = function() new("UndefinedType")

#' @rdname UndefinedType
#' @exportClass UndefinedType
setClass("UndefinedType", contains = "Type")
