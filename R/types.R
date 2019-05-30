# Class definitions for types that are not language-specific.

#' @include terms.R
#' @include type_environment.R
NULL

#' @exportClass typesys::Vector
setClass("typesys::Vector", contains = "typesys::Composite")
setValidity("typesys::Vector", function(object) {
  if (length(object@components) == 1L)
    TRUE
  else
    "must have exactly 1 type argument"
})

#' @export
Vector = function(...) {
  new("typesys::Vector", args = list(...))
}


#' @exportClass typesys::Function
setClass("typesys::Function", contains = "typesys::Composite")
setValidity("typesys::Function", function(object) {
  if (length(object@components) > 0L)
    TRUE
  else
    "must have return type (at least 1 component)"
})

Function = function(..., return_type) {
  components = list(...)
  len = length(components)

  # Check if argument types are in a list.
  if (len == 1L && is.list(components[[1L]])) {
    components = components[[1L]]
    len = length(components)
  }

  # Check if return type is separate.
  if (!missing(return_type)) {
    len = len + 1L
    components[[len]] = return_type
  }

  new("typesys::Function", components = components)
}

## #' Function Type
## #'
## #' A function.
## #'
## #' @export
## FunctionType = function(args, return_type) {
##   if (!is(args, "typesys::ParameterType"))
##     args = ParameterType(args)
## 
##   quantified = union(args@quantified, return_type@quantified)
## 
##   new("typesys::FunctionType", args = args, return_type = return_type,
##     quantified = quantified, type_environment = NULL)
## }
## 
## setClassUnion("NullableTypeEnvironment", c("NULL", "TypeEnvironment"))
## 
## #' @rdname FunctionType
## #' @exportClass typesys::FunctionType
## setClass("typesys::FunctionType", contains = "typesys::Type",
##   slots = list(
##     args = "typesys::ParameterType",
##     return_type = "typesys::Type",
##     type_environment = "NullableTypeEnvironment"
##   )
## )
## 
## # Special Types ----------------------------------------
## 
## #' Join
## #'
## #' Take the join (least upper bound) of a list of types. This differs from a
## #' Union in that it always returns a named type.
## #'
## #' @export
## Join = function(..., simplify = TRUE) {
##   join = new("typesys::Join", args = list(...))
## 
##   if (simplify)
##     simplify(join)
##   else
##     join
## }
## 
## #' @rdname Join
## #' @exportClass typesys::Join
## setClass("typesys::Join", contains = "typesys::Type",
##   slots = list(
##     args = "list"
##   )
## )
## 
## # Composite Types ----------------------------------------
## 
## #' Composite Type
## #'
## #' A container for other (possibly composite) types. This is a superclass for
## #' structures, lists, arrays, and matrices.
## #'
## #' @slot types A named list of contained types.
## #'
## #' @name CompositeType-class
## #' @exportClass typesys::CompositeType
## setClass("typesys::CompositeType", contains = c("typesys::Type", "VIRTUAL"))
## 
## #' Array Type
## #'
## #' A dimensioned container for homogeneous types.
## #'
## #' @export
## ArrayType = function(type, dimension = list()) {
##   new("typesys::ArrayType", type = type, dimension = as.list(dimension))
## }
## 
## #' @rdname ArrayType
## #' @exportClass typesys::ArrayType
## setClass("typesys::ArrayType", contains = "typesys::CompositeType",
##   slots = list(
##     type = "typesys::Type",
##     dimension = "list"
##   )
## )
## 
## #' Vector Type
## #'
## #' A dimensioned container for homogeneous types. Unlike \code{ArrayType}, this
## #' class represents a container that can grow dynamically.
## #'
## #' @export
## VectorType = function(type, dimension = list()) {
##   new("typesys::VectorType", type = type, dimension = as.list(dimension))
## }
## 
## #' @rdname VectorType
## #' @exportClass typesys::VectorType
## setClass("typesys::VectorType", contains = "typesys::ArrayType")
## 
## 
## #' Record Type
## #'
## #' A container for heterogeneous types.
## #'
## #' @export
## RecordType = function(...) {
##   new("typesys::RecordType", fields = list(...))
## }
## 
## #' @rdname RecordType
## #' @exportClass typesys::RecordType
## setClass("typesys::RecordType", contains = "typesys::CompositeType",
##   slots = list(
##     fields = "list"
##   )
## )
## 
## 
## #' Parameter Type
## #'
## #' A list of parameters for a function.
## #'
## #' @export
## ParameterType = function(...) {
##   # Check if ... is a list or TypeEnvironment.
##   fields = list(...)
##   if (length(fields) == 1 && is(fields[[1]], "TypeEnvironment"))
##     fields = fields[[1]]$objects
## 
##   # Check quantification.
##   if (length(fields) == 0) {
##     quantified = character(0)
##   } else {
##     quantified = lapply(fields, function(a) a@quantified)
##     quantified = unique(unlist(quantified))
##   }
## 
##   new("typesys::ParameterType", fields = fields, quantified = quantified)
## }
## 
## #' @rdname ParameterType
## #' @exportClass typesys::ParameterType
## setClass("typesys::ParameterType", contains = "typesys::RecordType",
##   slots = list(
##     optional = "logical"
##   )
## )
## 
## 
## # Atomic Types ----------------------------------------
## 
## #' Atomic Type
## #'
## #' A monomorphic scalar type.
## #'
## #' @name AtomicType-class
## #' @exportClass typesys::AtomicType
## setClass("typesys::AtomicType", contains = c("typesys::Type", "VIRTUAL"))
