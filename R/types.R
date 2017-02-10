# Description:
#   S4 classes for types.

# TODO:
#   * Add method for setting element types.
#   * Update validity for composite types.


#' @include generics.R
NULL


# NOTE: This base class is a workaround because R assumes empty base classes
# are virtual.
setClass("Value")

#' Unknown Value
#'
#' Basic unknown value class. Later this could be expanded to a reference
#' system.
#' @export
UnknownValue = function() new("UnknownValue")
setClass("UnknownValue", contains = "Value")


#' Type
#'
#' Parent class for all types.
#'
#' @slot value A value, for constant folding
#' @slot contexts (character) Contextual information about the type.
#' @exportClass Type
setClass("Type", contains = "VIRTUAL",
  slots = list(
    contexts = "character",
    value = "ANY"
  ),
  prototype = list(
    contexts = character(0),
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
    types = "list"
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


# Methods
# =======

#' @export
setMethod("show", signature(object = "Type"),
  function(object) cat(format(object, indent = 0), "\n")
)


# Print out 
#   RecordType ()
#     IntegerType (index)
#
#' @export
setMethod("format", signature(x = "Type"),
  function(x, indent = 0, ...) {
    type_msg = class(x)[[1]]

    dim = dim(x)
    dim_msg =
      if (is.null(dim)) ""
      else sprintf(" [%s]", paste0(dim, collapse = "x"))

    contexts_msg = paste0(x@contexts, collapse = ", ")

    sprintf("%*s%s%s {%s}", indent, "", type_msg, dim_msg, contexts_msg)
  }
)


#' @export
setMethod("format", signature(x = "CompositeType"),
  function(x, indent = 0, ...) {
    types_msg = vapply(x@types, format, character(1), indent = indent + 2)
    types_msg = paste0(types_msg, collapse = "\n")

    sprintf("%s\n%s", callNextMethod(), types_msg)
  }
)


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
