#' @include types.R
NULL

setClass("typesys::NullType", contains = "typesys::Constant")
setClass("typesys::EnvironmentType", contains = "typesys::Constant")
# Single character (like C "character" type):
setClass("typesys::CharacterType", contains = "typesys::Constant")
# Multiple characters (like scalar R "character" type):
setClass("typesys::StringType", contains = "typesys::Constant")
setClass("typesys::LogicalType", contains = "typesys::Constant")
setClass("typesys::IntegerType", contains = "typesys::Constant")
# Double-precision floating point:
setClass("typesys::NumericType", contains = "typesys::Constant")
setClass("typesys::ComplexType", contains = "typesys::Constant")
# TODO: list
setClass("typesys::ExternalPtrType", contains = "typesys::Constant")
setClass("typesys::RawType", contains = "typesys::Constant")

#' @export
NullType = new("typesys::NullType")

#' @export
EnvironmentType = new("typesys::EnvironmentType")

#' @export
CharacterType = new("typesys::CharacterType")

#' @export
LogicalType = new("typesys::LogicalType")

#' @export
IntegerType = new("typesys::IntegerType")

#' @export
NumericType = new("typesys::NumericType")

#' @export
ComplexType = new("typesys::ComplexType")

#' @export
StringType = new("typesys::StringType")

#' @export
ExternalPtrType = new("typesys::ExternalPtrType")

#' @export
RawType = new("typesys::RawType")

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
