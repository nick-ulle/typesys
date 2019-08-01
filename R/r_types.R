#' @include terms.R
NULL

#' @exportClass typesys::RVector
setClass("typesys::RVector", contains = "typesys::Composite")
setValidity("typesys::RVector", function(object) {
  if (length(object@components) == 1L)
    TRUE
  else
    "must have exactly 1 type argument"
})

#' @export
RVector = function(...) {
  new("typesys::RVector", components = list(...))
}


#' @exportClass typesys::RFunction
setClass("typesys::RFunction", contains = "typesys::Function")

#' @export
RFunction = function(...) {
  new("typesys::RFunction", Function(...))
}

# Constants ----------------------------------------

setClass("typesys::RNull", contains = "typesys::Constant")
setClass("typesys::REnvironment", contains = "typesys::Constant")
# Single character (like C "char" type):
setClass("typesys::RChar", contains = "typesys::Constant")
# Multiple characters (like scalar R "character" type):
setClass("typesys::RString", contains = "typesys::Constant")
setClass("typesys::RLogical", contains = "typesys::Constant")
setClass("typesys::RInteger", contains = "typesys::Constant")
# Double-precision floating point:
setClass("typesys::RNumeric", contains = "typesys::Constant")
setClass("typesys::RComplex", contains = "typesys::Constant")
# TODO: list
setClass("typesys::RExternalPtr", contains = "typesys::Constant")
setClass("typesys::RRaw", contains = "typesys::Constant")

#' @export
RNull = new("typesys::RNull")

#' @export
REnvironment = new("typesys::REnvironment")

#' @export
RChar = new("typesys::RChar")

#' @export
RLogical = new("typesys::RLogical")

#' @export
RInteger = new("typesys::RInteger")

#' @export
RNumeric = new("typesys::RNumeric")

#' @export
RComplex = new("typesys::RComplex")

#' @export
RString = new("typesys::RString")

#' @export
RExternalPtr = new("typesys::RExternalPtr")

#' @export
RRaw = new("typesys::RRaw")

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
