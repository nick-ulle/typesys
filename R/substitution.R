#' @include types.R
#' @include type_environment.R
NULL

#' @export
Substitution = function(name, value) {
  sub = structure(list(), class = "Substitution")

  if (!missing(name) && !missing(value)) {
    sub[[name]] = value
  }

  sub
}

#' Compose Substitutions
#' 
#' This function applies the second substitution to the first.
#'
#' @export
compose =
function(sub1, sub2) {
  x = structure(lapply(sub1, applySubstitution, sub2), class = "Substitution")

  new = setdiff(names(sub2), names(sub1))
  x[new] = sub2[new]

  x
}

#' @export
applySubstitution = function(exp, sub) {
  UseMethod("applySubstitution")
}

#' Apply a Substitution
#'
#' @export
setGeneric("applySubstitution")


#' @export
applySubstitution.TypeEnvironment =
function(exp, sub) {
  exp$env = lapply(exp$env, applySubstitution, sub)
  exp
}

#' @export
#setMethod("applySubstitution", "typesys::TypeEnvironment",
#  `applySubstitution.typesys::TypeEnvironment`)


#' @export
`applySubstitution.typesys::FunctionType` =
function(exp, sub) {
  exp@args = lapply(exp@args, applySubstitution, sub)
  exp@return_type = applySubstitution(exp@return_type, sub)

  exp
}

#' @export
setMethod("applySubstitution", "typesys::FunctionType",
  `applySubstitution.typesys::FunctionType`)


#' @export
`applySubstitution.typesys::TypeVar` =
function(exp, sub) {
  index = match(exp@name, names(sub))
  if (is.na(index))
    exp
  else
    sub[[index]]
}

#' @export
setMethod("applySubstitution", "typesys::TypeVar",
  `applySubstitution.typesys::TypeVar`)

#' @export
`applySubstitution.typesys::AtomicType` =
function(exp, sub) exp

#' @export
setMethod("applySubstitution", "typesys::AtomicType",
  `applySubstitution.typesys::AtomicType`)