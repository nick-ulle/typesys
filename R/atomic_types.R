
#' @include types.R
NULL

# Numeric Types
# =============

setClass("NumberType", contains = c("AtomicType", "VIRTUAL"))

.IntegerType =
  setClass("IntegerType", contains = "AtomicType")
.RealType =
  setClass("RealType", contains = "AtomicType")
.ComplexType =
  setClass("ComplexType", contains = "AtomicType")

#' @export
IntegerType = function(...) .IntegerType(...)
#' @export
RealType = function(...) .RealType(...)
#' @export
ComplexType = function(...) .ComplexType(...)


# String Types
# ============

setClass("TextType", contains = c("AtomicType", "VIRTUAL"))

.StringType =
  setClass("StringType", contains = "TextType")
.CharacterType =
  setClass("CharacterType", contains = "TextType") 

#' @export
StringType = function(...) .StringType(...)
#' @export
CharacterType = function(...) .CharacterType(...)


# Other Atomic Types
# ==================

.NullType =
  setClass("NullType", contains = "AtomicType")

.BooleanType =
  setClass("BooleanType", contains = "AtomicType")

.FunctionType =
  setClass("FunctionType", contains = "AtomicType")

#' @export
NullType = function(...) .NullType(...)
#' @export
BooleanType = function(...) .BooleanType(...)
#' @export
FunctionType = function(...) .FunctionType(...)

