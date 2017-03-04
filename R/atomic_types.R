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

  new("typesys::FunctionType", return_type = return_type, scope = scope)
}


#' @rdname FunctionType
#' @exportClass typesys::FunctionType
setClass("typesys::FunctionType", contains = "typesys::AtomicType",
  slots = list(
    return_type = "typesys::Type",
    scope = "typesys::MaybeScope"
  )
)


#' @export
setMethod("format", signature(x = "typesys::FunctionType"),
  function(x, indent = 0, ...) {
    return_msg = format(x@return_type, indent + 4)
    return_msg = sprintf("%*s@return_type\n%s", indent + 2, "", return_msg)

    scope_msg = sprintf("%*s@scope %s", indent + 2, "", "<TODO>")

    sprintf("%s\n%s\n\n%s", callNextMethod(), return_msg, scope_msg)
  }
)


# Numeric Types
# =============

setClass("typesys::NumberType", contains = c("typesys::AtomicType", "VIRTUAL"))

#' @export
IntegerType = function(...) new("typesys::IntegerType", ...)
setClass("typesys::IntegerType", contains = "typesys::NumberType")

#' @export
RealType = function(...) new("typesys::RealType", ...)
setClass("typesys::RealType", contains = "typesys::NumberType")

#' @export
ComplexType = function(...) new("typesys::ComplexType", ...)
setClass("typesys::ComplexType", contains = "typesys::NumberType")


# String Types
# ============

setClass("typesys::TextType", contains = c("typesys::AtomicType", "VIRTUAL"))

#' @export
StringType = function(...) new("typesys::StringType", ...)
setClass("typesys::StringType", contains = "typesys::TextType")

#' @export
CharacterType = function(...) new("typesys::CharacterType", ...)
setClass("typesys::CharacterType", contains = "typesys::TextType")


# Other Atomic Types
# ==================

#' @export
NullType = function(...) new("typesys::NullType", ...)
setClass("typesys::NullType", contains = "typesys::AtomicType")

#' @export
BooleanType = function(...) new("typesys::BooleanType", ...)
setClass("typesys::BooleanType", contains = "typesys::AtomicType")
