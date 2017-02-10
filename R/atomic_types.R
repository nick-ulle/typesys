# Description:
#
# Definitions for concrete, non-parametric types.


#' @include types.R
#' @include scope.R
NULL

# Function Type
# =============

#' Function Type
#'
#' A function.
#'
#' @export
FunctionType = function(return_type, scope) {
  if (missing(scope))
    scope = NULL

  new("FunctionType", return_type = return_type, scope = scope)
}


#' @rdname FunctionType
#' @exportClass FunctionType
setClass("FunctionType", contains = "AtomicType",
  slots = list(
    return_type = "Type",
    scope = "MaybeScope"
  )
)


#' @export
setMethod("format", signature(x = "FunctionType"),
  function(x, indent = 0, ...) {
    return_msg = format(x@return_type, indent + 4)
    return_msg = sprintf("%*s@return_type\n%s", indent + 2, "", return_msg)

    scope_msg = sprintf("%*s@scope %s", indent + 2, "", "<TODO>")

    sprintf("%s\n%s\n\n%s", callNextMethod(), return_msg, scope_msg)
  }
)


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

#' @export
NullType = function(...) .NullType(...)
#' @export
BooleanType = function(...) .BooleanType(...)
