# Type Environments keep track of which type variables have been added.

#' Type Environment
#'
#' @export
TypeEnvironment = R6::R6Class("TypeEnvironment",
  "public" = list(
    parent = NULL,
    active = character(0),
    bound_type_vars = character(0),
    env = list(),

    initialize = function(..., parent = NULL) {
      env = list(...)
      if (length(env) == 1 && is(env[[1]], "list"))
        env = env[[1]]

      self$env = lapply(env, formula_to_type)

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
  if (is(value, "typesys::TypeVar"))
    x$bound_type_vars = union(x$bound_type_vars, value@name)

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
