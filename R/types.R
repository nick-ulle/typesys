# Description:
#   S4 classes for types.

# TODO:
#   * Add method for setting element types.
#   * Update validity for composite types.


#' @include generics.R
NULL


#' Unknown Value
#'
#' Basic unknown value class. Later this could be expanded to a reference
#' system.
#' @export
UnknownValue = function() new("UnknownValue")
setClass("UnknownValue", contains = "NULL")


#' Type
#'
#' Parent class for all types.
#'
#' @slot value A value, for constant folding
#' @exportClass Type
setClass("Type", contains = "VIRTUAL",
  slots = list(
    contexts = "character",
    value = "ANY"
  ),
  prototype = list(
    contexts = "",
    value = UnknownValue()
  )
)


#' Atomic Type
#'
#' A monomorphic scalar type.
#'
#' @exportClass AtomicType
setClass("AtomicType", contains = c("Type", "VIRTUAL")) 


setClassUnion("list|Type", c("list", "Type"))


#' Composite Type
#'
#' A container for other (possibly composite) types. This is a superclass for
#' structures, lists, arrays, and matrices.
#'
#' @slot types A named list of contained types.
#' @exportClass CompositeType
setClass("CompositeType", contains = c("Type", "VIRTUAL"),
  slots = list(
    types = "list|Type"
  )
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


#' Union Type
#'
#' A union of two or more types.
#'
#' @export
UnionType = function(...) {
  new("UnionType", types = list(...))
}

#' @slot types A list of possible types.
#' @rdname UnionType
#' @exportClass UnionType
setClass("UnionType", contains = "Type",
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


# Methods
# =======

#' @export
setMethod("same_type", signature(x = "Type"),
  function(x, y) is(x, class(y))
)


#' @export
setMethod("same_type", signature(x = "CompositeType"),
  function(x, y) {
    same_class = is(x, class(y))
    if (!same_class)
      return(FALSE)

    types_x = element_type_all(x)
    types_y = element_type_all(y)
    if (length(types_x) != length(types_y))
      return(FALSE)

    all(mapply(same_type, types_x, types_y))
  }
)


#' @export
setMethod("element_type", signature(self = "Type"),
  function(self) self
)


#' @export
setMethod("element_type", signature(self = "CompositeType"),
  function(self) stop("composite types may contain multiple element types.")
)
    

#' @export
setMethod("element_type_all", signature(self = "Type"),
  function(self) list(self)
)


#' @export
setMethod("element_type_all", signature(self = "CompositeType"),
  function(self) self@types
)


#' @describeIn Type Length of the type.
#' @export
setMethod("length", signature(x = "Type"),
  function(x) 1L
)


#' @describeIn CompositeType Length of the type.
#' @export
setMethod("length", signature(x = "CompositeType"),
  function(x) length(x@types)
)


# Functions
# =========

#' Add Context to Type
#'
#' @export
add_context = function(object, context) {
  object@contexts = c(object@contexts, context)
  validObject(object)
  return(object)
}


#' Check If Type Has Context
#'
#' @export
has_context = function(object, context) {
  context %in% object@contexts
}
