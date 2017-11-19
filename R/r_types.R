#' @include types.R
NULL

#' @export
NullType = function(...) new("typesys::NullType", ...)
setClass("typesys::NullType", contains = "typesys::AtomicType")

#' @export
EnvironmentType = function(...) new("typesys::EnvironmentType", ...)
setClass("typesys::EnvironmentType", contains = "typesys::AtomicType")

# Single characters, e.g., a C "character".
#' @export
CharacterType = function(...) new("typesys::CharacterType", ...)
setClass("typesys::CharacterType", contains = "typesys::AtomicType")

#' @export
LogicalType = function(...) new("typesys::LogicalType", ...)
setClass("typesys::LogicalType", contains = "typesys::AtomicType")

#' @export
IntegerType = function(...) new("typesys::IntegerType", ...)
setClass("typesys::IntegerType", contains = "typesys::AtomicType")

# Double-precision floating point.
#' @export
NumericType = function(...) new("typesys::NumericType", ...)
setClass("typesys::NumericType", contains = "typesys::AtomicType")

#' @export
ComplexType = function(...) new("typesys::ComplexType", ...)
setClass("typesys::ComplexType", contains = "typesys::AtomicType")

# A string, e.g., an R "character".
#' @export
StringType = function(...) new("typesys::StringType", ...)
setClass("typesys::StringType", contains = "typesys::AtomicType")

# list:	a list

#' @export
ExternalPtrType = function(...) new("typesys::ExternalPtrType", ...)
setClass("typesys::ExternalPtrType", contains = "typesys::AtomicType")

#' @export
RawType = function(...) new("typesys::StringType", ...)
setClass("typesys::StringType", contains = "typesys::AtomicType")


# Low Priority ----------------------------------------
# S4: an S4 object which is not a simple object
# symbol: a variable name
# pairlist: a pairlist object (mainly internal)
# promise: an object used to implement lazy evaluation
# language: an R language construct
# weakref: a weak reference object
# bytecode: byte code (internal only) ***
# expression:	an expression object


# NOTE: For now these can be treated as FunctionType
# special: an internal function that does not evaluate its arguments
# builtin: an internal function that evaluates its arguments
