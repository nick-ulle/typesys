# Class definitions for types that are not language-specific.

#' @include type_environment.R
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
    quantified = "character"
  ),
  prototype = list(
    contexts = character(0),
    quantified = character(0)
  )
)


#' Type Variable
#'
#' @export
TypeVariable = function(name, quantify = FALSE) {
  if (quantify)
    new("typesys::TypeVariable", name = name, quantified = name)
  else
    new("typesys::TypeVariable", name = name)
}

#' @rdname TypeVariable
#' @exportClass typesys::TypeVariable
setClass("typesys::TypeVariable", contains = "typesys::Type",
  slots = list(
    name = "character"
  )
)


#' Function Type
#'
#' A function.
#'
#' @export
FunctionType = function(args, return_type)
  UseMethod("FunctionType")

#' @export
FunctionType.list = function(args, return_type) {
  # FIXME: Test this constructor and make it less hacky.
  if (length(args) > 0) {
    quantified = lapply(args, function(a) a@quantified)
    quantified = unique(unlist(quantified))
  } else
    quantified = character(0)

  new("typesys::FunctionType", args = args, return_type = return_type,
    quantified = quantified, type_environment = NULL)
}

#' @export
FunctionType.TypeEnvironment = function(args, return_type)
  FunctionType.list(args$objects, return_type)

#' @export
FunctionType.default = function(args, return_type)
  FunctionType.list(list(args), return_type)

setClassUnion("NullableTypeEnvironment", c("NULL", "TypeEnvironment"))

#' @rdname FunctionType
#' @exportClass typesys::FunctionType
setClass("typesys::FunctionType", contains = "typesys::Type",
  slots = list(
    args = "list",
    return_type = "typesys::Type",
    type_environment = "NullableTypeEnvironment"
  )
)

# Special Types ----------------------------------------

#' Join
#'
#' Take the join (least upper bound) of a list of types. This differs from a
#' Union in that it always returns a named type.
#'
#' @export
Join = function(..., simplify = TRUE) {
  join = new("typesys::Join", args = list(...))

  if (simplify)
    simplify(join)
  else
    join
}

#' @rdname Join
#' @exportClass typesys::Join
setClass("typesys::Join", contains = "typesys::Type",
  slots = list(
    args = "list"
  )
)

# Composite Types ----------------------------------------

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


# Atomic Types ----------------------------------------

#' Atomic Type
#'
#' A monomorphic scalar type.
#'
#' @name AtomicType-class
#' @exportClass typesys::AtomicType
setClass("typesys::AtomicType", contains = c("typesys::Type", "VIRTUAL"))
