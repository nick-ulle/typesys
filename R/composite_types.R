
#' @include types.R
NULL

#' Record Type
#'
#' A container for heterogeneous types.
#'
#' @export
RecordType = function(types) {
  new("RecordType", types = types)
}

#' @rdname RecordType
#' @exportClass RecordType
setClass("RecordType", contains = "CompositeType",
  slots = list(
    types = "list"
  ),
  validity = function(object) {
    messages = character(0)

    is_type = sapply(object@types, is, "Type")
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
  new("ArrayType", types = type, dimension = as.integer(dimension))
}

#' @rdname ArrayType
#' @exportClass ArrayType
setClass("ArrayType", contains = "CompositeType",
  slots = list(
    # Cannot restrict to AtomicType because of UnknownType.
    types = "Type",
    dimension = "integer"
  ),
  validity = function(object) {
    messages = character(0)

    if (length(object@dimension) < 1)
      messages = c(messages, "dimension must have length >= 1.")

    if (!is.na(object@dimension) && any(object@dimension < 0))
      messages = c(messages, "dimension must be >= 0.")

    if (length(messages) > 0) messages
    else TRUE
  }
)


#' @export
setMethod("element_type", signature(self = "ArrayType"),
  function(self) self@types
)


#' @export
setMethod("element_type<-", signature(self = "ArrayType"),
  function(self, value) {
    self@types = value
    validObject(self)
    return(self)
  }
)


#' @export
setMethod("element_type_all", signature(self = "ArrayType"),
  function(self) list(self@types)
)


#' @export
setMethod("length", signature(x = "ArrayType"),
  function(x) prod(x@dimension)
)


#' @export
setMethod("dim", signature(x = "ArrayType"),
  function(x) x@dimension
)
