# Description:
#
# Definitions for concrete, parametric types.


#' @include types.R
NULL


#' Record Type
#'
#' A container for heterogeneous types.
#'
#' @export
RecordType = function(types) {
  new("typesys::RecordType", types = types)
}

#' @rdname RecordType
#' @exportClass typesys::RecordType
setClass("typesys::RecordType", contains = "typesys::CompositeType",
  validity = function(object) {
    messages = character(0)

    is_type = sapply(object@types, is, "typesys::Type")
    if (!all(is_type))
      messages = c(messages, "types must have superclass Type.")

    if (length(messages) > 0) messages
    else TRUE
  }
)


#' Array Type
#'
#' A dimensioned container for homogeneous types. This is a superclass for
#' matrices.
#'
#' @export
ArrayType = function(type, dimension) {
  new("typesys::ArrayType",
    types = list(type), dimension = as.integer(dimension)
  )
}

#' @rdname ArrayType
#' @exportClass typesys::ArrayType
setClass("typesys::ArrayType", contains = "typesys::CompositeType",
  slots = list(
    dimension = "integer"
  ),
  validity = function(object) {
    messages = character(0)

    if (!is(object@types[[1]], "typesys::Type"))
      messages = c(messages, "type must extend class typesys::Type.")

    if (length(object@dimension) < 1)
      messages = c(messages, "dimension must have length >= 1.")

    if (!is.na(object@dimension) && any(object@dimension < 0))
      messages = c(messages, "dimension must be >= 0.")

    if (length(messages) > 0) messages
    else TRUE
  }
)


#' @export
setMethod("element_type", signature(self = "typesys::ArrayType"),
  function(self) self@types[[1]]
)


#' @export
setMethod("element_type<-", signature(self = "typesys::ArrayType"),
  function(self, value) {
    self@types = list(value)
    validObject(self)
    return(self)
  }
)


#' @export
setMethod("length", signature(x = "typesys::ArrayType"),
  function(x) prod(x@dimension)
)


#' @export
setMethod("dim", signature(x = "typesys::ArrayType"),
  function(x) x@dimension
)
