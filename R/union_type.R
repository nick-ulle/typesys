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
