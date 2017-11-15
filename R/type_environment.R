#' Type Environment
#'
#' A type environment maps names to types. The idea is analagous to how an R
#' environment maps names to values.
#'
#' @export
TypeEnvironment = R6::R6Class("TypeEnvironment",
  "public" = list(
    parent = NULL,
    active = character(0),
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

setOldClass("TypeEnvironment")

#' @export
`[.TypeEnvironment` = function(x, i) {
  new_env = TypeEnvironment$new()
  new_env$env = x$env[i]
  new_env
}

#' @export
`[[.TypeEnvironment` = function(x, i) {
  x$env[[i]]
}

#' @export
`[[<-.TypeEnvironment` = function(x, i, value) {
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
