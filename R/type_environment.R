# Type Environments keep track of which type variables have been added.

#' Type Environment
#'
#' @export
TypeEnvironment = function(name, value) {
  x = new("typesys::TypeEnvironment")

  if (!missing(name) && !missing(value))
    x@env[[name]] = value

  x
}

#' @rdname TypeEnvironment
#' @exportClass typesys::TypeEnvironment
setClass("typesys::TypeEnvironment",
  slots = list(
    variables = "character",
    env = "list"
  ),

  prototype = list(
    variables = character(0),
    env = list()
  )
)

#' @export
`[[.typesys::TypeEnvironment` = function(x, i) {
  x@env[[i]]
}

#' @export
setMethod("[[", "typesys::TypeEnvironment", `[[.typesys::TypeEnvironment`)

#' @export
`[[<-.typesys::TypeEnvironment` = function(x, i, value) {
  #vars = collectVars(value)
  #x$vars = union(x$vars, vars)
  x@env[[i]] = value
  x
}

#' @export
setMethod("[[<-", "typesys::TypeEnvironment", `[[<-.typesys::TypeEnvironment`)


#' @export
`length.typesys::TypeEnvironment` = function(x) {
  length(x@env)
}

#' @export
setMethod("length", "typesys::TypeEnvironment",
  `length.typesys::TypeEnvironment`)


#' @export
`names.typesys::TypeEnvironment` = function(x) {
  names(x@env)
}

#' @export
setMethod("names", "typesys::TypeEnvironment",
  `names.typesys::TypeEnvironment`)
