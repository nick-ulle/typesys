# Description:
#
# Abstract base class definitions for types.


#' @include generics.R
#' @include values.R
NULL


#' Type
#'
#' Parent class for all types.
#'
#' @slot value A value, for constant folding
#' @slot contexts (character) Contextual information about the type.
#'
#' @name Type-class
#' @exportClass typesys::Type
setClass("typesys::Type", contains = "VIRTUAL",
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
#' @name AtomicType-class
#' @exportClass typesys::AtomicType
setClass("typesys::AtomicType", contains = c("typesys::Type", "VIRTUAL"))


setClassUnion("typesys::list|Type", c("list", "typesys::Type"))


#' Composite Type
#'
#' A container for other (possibly composite) types. This is a superclass for
#' structures, lists, arrays, and matrices.
#'
#' @slot types A named list of contained types.
#'
#' @name CompositeType-class
#' @exportClass typesys::CompositeType
setClass("typesys::CompositeType", contains = c("typesys::Type", "VIRTUAL"),
  slots = list(
    types = "list"
  )
)


# Methods
# =======

#' @export
setMethod("show", signature(object = "typesys::Type"),
  function(object) cat(format(object, indent = 0), "\n")
)


# Print out 
#   RecordType ()
#     IntegerType (index)

#' @export
setMethod("format", signature(x = "typesys::Type"),
  function(x, indent = 0, ...) {
    type_msg = class(x)[[1]]

    dim = dim(x)
    dim_msg =
      if (is.null(dim)) ""
      else {
        dim = vapply(dim, format, "")
        sprintf(" [%s]", paste0(dim, collapse = " x "))
      }

    contexts_msg = paste0(x@contexts, collapse = ", ")

    sprintf("%*s%s%s {%s}", indent, "", type_msg, dim_msg, contexts_msg)
  }
)


#' @export
setMethod("format", signature(x = "typesys::CompositeType"),
  function(x, indent = 0, ...) {
    types_msg = vapply(x@types, format, character(1), indent = indent + 2)
    types_msg = paste0(types_msg, collapse = "\n")

    sprintf("%s\n%s", callNextMethod(), types_msg)
  }
)


#' @export
setMethod("same_type", signature(x = "typesys::Type"),
  function(x, y) is(x, class(y))
)


#' @export
setMethod("same_type", signature(x = "typesys::CompositeType"),
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
setMethod("element_type", signature(self = "typesys::Type"),
  function(self) self
)


#' @export
setMethod("element_type", signature(self = "typesys::CompositeType"),
  function(self) stop("composite types may contain multiple element types.")
)
    

#' @export
setMethod("element_type_all", signature(self = "typesys::Type"),
  function(self) list(self)
)


#' @export
setMethod("element_type_all", signature(self = "typesys::CompositeType"),
  function(self) self@types
)


#' @export
setMethod("length", signature(x = "typesys::Type"),
  function(x) 1L
)


#' @export
setMethod("length", signature(x = "typesys::CompositeType"),
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
