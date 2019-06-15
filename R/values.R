
#' @include generics.R
NULL

# NOTE: This base class is a workaround because R assumes empty base classes
# are virtual.
setClass("typesys::Value")

#' Unknown Value
#'
#' Basic unknown value class. Later this could be expanded to a reference
#' system.
#'
UnknownValue = function() new("typesys::UnknownValue")
setClass("typesys::UnknownValue", contains = "typesys::Value")

#' Symbol Value
#'
#' A symbol.
#'
SymbolValue = function(name) new("typesys::SymbolValue", name = name)
setClass("typesys::SymbolValue", contains = "typesys::Value",
  slots = list(
    name = "character")
  )


setMethod("format", signature(x = "typesys::UnknownValue"),
  function(x, indent = 0, ...) "?")

setMethod("format", signature(x = "typesys::SymbolValue"),
  function(x, indent = 0, ...) x@name)
