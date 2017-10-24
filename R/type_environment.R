# Type Environments keep track of which type variables have been added.

#' Type Environment
#'
#' @export
TypeEnvironment = R6::R6Class("TypeEnvironment",
  "public" = list(
    parent = NULL,
    active = character(0),
    env = list(),

    initialize = function(name, value, parent = NULL) {
      if (!missing(name) && !missing(value))
        self$env[[name]] = value

      self$parent = parent
    }
  )
)

#' @export
`[[.TypeEnvironment` = function(x, i) {
  x$env[[i]]
}

#' @export
`[[<-.TypeEnvironment` = function(x, i, value) {
  #vars = collectVars(value)
  #x$vars = union(x$vars, vars)
  x$env[[i]] = value
  x
}

#' @export
length.TypeEnvironment = function(x) {
  length(x$env)
}

#' @export
names.TypeEnvironment = function(x) {
  names(x$env)
}
