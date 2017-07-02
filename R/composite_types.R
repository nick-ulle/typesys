# Description:
#
# Definitions for concrete, parametric types.


#' @include types.R
NULL


#' List Type
#'
#' A container for heterogeneous types.
#'
#' @export
ListType = function(types) {
  new("typesys::ListType", types = types)
}

#' @rdname ListType
#' @exportClass typesys::ListType
setClass("typesys::ListType", contains = "typesys::CompositeType",
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
ArrayType = function(type, dimension = list(UnknownValue())) {
  new("typesys::ArrayType",
    types = list(type), dimension = as.list(dimension)
  )
}

#' @rdname ArrayType
#' @exportClass typesys::ArrayType
setClass("typesys::ArrayType", contains = "typesys::CompositeType",
  slots = list(
    dimension = "list"
  ),
  validity = function(object) {
    messages = character(0)

    if (!is(object@types[[1]], "typesys::Type"))
      messages = c(messages, "type must extend class typesys::Type.")

    if (length(object@dimension) < 1)
      messages = c(messages, "dimension must have length >= 1.")

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
  function(x) {
    Reduce(function(a, b) {
      if (is.numeric(b)) b * a
      else NA
    }, x@dimension)
  }
)


#' @export
setMethod("dim", signature(x = "typesys::ArrayType"),
  function(x) x@dimension
)



setClass("typesys::SEXPType", contains = "typesys::CompositeType")
SEXPType = function(...) new("typesys::SEXPType", ...)

setClass("typesys::LISTSEXPType", contains = "typesys::SEXPType")
LISTSEXPType = function(...) new("typesys::LISTSEXPType", ...)
               
