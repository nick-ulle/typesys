# Description:
#   Legacy types for RTypeInferance compatibility.


# Semantic Types
# ==============
# Also see conditional types.

setClass("SemanticType", contains = c("Type", "VIRTUAL"),
  slots = list(
    atom = "AtomicType"
  )
)

IteratorType =
  setClass("IteratorType", contains = "SemanticType")


# Value Operations
# ================


# TODO: Check if value is a vector.
#' Get Values
#'
#' @export
setGeneric("value",
  function(self, unknown) standardGeneric("value")
)

#' @export
setMethod("value", signature(self = "AtomicType", unknown = "missing"),
  function(self, unknown) self@value
)

#' @export
setMethod("value", signature(self = "Type"),
  function(self, unknown) {
    if (is(self@value, "UnknownValue"))
      unknown
    else
      self@value
  }
)

#setMethod("value",
#  signature(self = "SemanticType"),
#  function(self, unknown) {
#    self = atomicType(self)
#    callGeneric()
#  }
#)

#' Assign Values
#'
#' @export
setGeneric("value<-", function(self, value) standardGeneric("value<-"))

#' @export
setMethod("value<-", signature(self = "Type"),
  function(self, value) {
    self@value = value
    return(self)
  }
)


# Methods

#setMethod("identicalType",
#  signature(x = "SemanticType"),
#  function(x, y) {
#    is_identical = is(x, class(y))
#
#    is_identical = 
#      is_identical &
#      sapply(slotNames(x),
#        function(name) {
#          identicalType(slot(x, name), slot(y, name))
#        }
#      )
#
#    all(is_identical)
#  }
#)
#
#setGeneric("atomicType",
#  function(self) standardGeneric("atomicType"),
#  valueClass = "AtomicType")
#
#setMethod("atomicType",
#  signature(self = "AtomicType"),
#  function(self) self
#)
#
#setMethod("atomicType",
#  signature(self = "SemanticType"),
#  function(self) self@atom
#)
#
#setGeneric("atomicType<-",
#  function(self, value) standardGeneric("atomicType<-"),
#  valueClass = "Type")
#
#setMethod("atomicType<-",
#  signature(self = "SemanticType"),
#  function(self, value) {
#    self@atom = value
#    return(self)
#  }
#)

#setMethod("length",
#  signature(x = "Type"),
#  function(x) 1L
#)

# Atomic Types

#setMethod("length",
#  signature(x = "UnknownType"),
#  function(x) NA_integer_
#)

#NullType =
#  setClass("NullType", contains = "AtomicType")
#
#LogicalType =
#  setClass("LogicalType", contains = "AtomicType")
#IntegerType =
#  setClass("IntegerType", contains = "AtomicType")
#NumericType =
#  setClass("NumericType", contains = "AtomicType")
#ComplexType =
#  setClass("ComplexType", contains = "AtomicType")
#CharacterType =
#  setClass("CharacterType", contains = "AtomicType")
#setClass("StringType", contains = "AtomicType")

# Semantic Types --------------------------------------------------

#VectorType =
#  function(atom, dimension) {
#    .VectorType(atom = atom, dimension = as.integer(dimension))
#  }
#
#.VectorType =
#  setClass("VectorType", contains = "SemanticType",
#    slots = list(
#      dimension = "integer"
#    )
#  )
#
#setMethod("length",
#  signature(x = "VectorType"),
#  function(x) prod(x@dimension)
#)
#
#setMethod("length<-",
#  signature(x = "VectorType"),
#  function(x, value) dim(x) = value
#)
#
#setMethod("dim",
#  signature(x = "VectorType"),
#  function(x) x@dimension
#)
#
#setMethod("dim<-",
#  signature(x = "VectorType"),
#  function(x, value) {
#    x@dimension = as.integer(value)
#    return(x)
#  }
#)

#setClass("ListType", contains = "ScalarType")

