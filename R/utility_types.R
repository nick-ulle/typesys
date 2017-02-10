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
  new("Union", types = list(...))
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
setMethod("format", signature(x = "Union"),
  function(x, indent = 0, ...) {
    sprintf("Union(%s)", paste(x@types, collapse = ", "))
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
