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
  u = new("typesys::Union", types = list(...))
  simplify(u)
}

#' @slot types A list of types.
#' @rdname Union
#' @exportClass typesys::Union
setClass("typesys::Union", contains = "typesys::Type",
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
setMethod("[[", signature(x = "typesys::Union"),
  function(x, i, ...) {
    return (x@types[[i]])
  }
)

#' @export
setMethod("length", signature(x = "typesys::Union"),
  function(x) {
    return (length(x@types))
  }
)

#' @export
setMethod("format", signature(x = "typesys::Union"),
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
  new("typesys::Call", func = func, args = list(...))
}

#' @slot func The name of the function to call
#' @slot args A list of argument types
#' @rdname Call
#' @exportClass typesys::Call
setClass("typesys::Call",
  slots = list(
    func = "character",
    args = "list"
  )
)

#' @export
setMethod("format", signature(x = "typesys::Call"),
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
UnknownType = function() new("typesys::UnknownType")

#' @rdname UnknownType
#' @exportClass typesys::UnknownType
setClass("typesys::UnknownType", contains = "typesys::Type")


# TODO: Incorporate conditional typing.
#' Undefined Type
#'
#' "Bottom" type in the type lattice, which indicates a variable which is
#' nonexistent or has not yet been initialized.
#'
#' @export
UndefinedType = function() new("typesys::UndefinedType")

#' @rdname UndefinedType
#' @exportClass typesys::UndefinedType
setClass("typesys::UndefinedType", contains = "typesys::Type")
