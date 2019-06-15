#' Type Environment
#'
#' A type environment maps names to types. The idea is analagous to how an R
#' environment maps names to values.
#'
TypeEnvironment = R6::R6Class("TypeEnvironment",
  "public" = list(
    parent = NULL,
    objects = list(),
    active = character(0),

    initialize = function(..., quantify = FALSE, parent = NULL) {
      objects = list(...)
      if (length(objects) == 1 && is(objects[[1]], "list"))
        objects = objects[[1]]

      if (length(unique(names(objects))) != length(objects))
        stop("Objects in a TypeEnvironment must have unique names.")

      self$objects = lapply(objects, formula_to_type, quantify)

      self$parent = parent
    }
  )
)

setOldClass("TypeEnvironment")

`[.TypeEnvironment` = function(x, i) {
  TypeEnvironment$new(x$objects[i])
}

`[[.TypeEnvironment` = function(x, i) {
  x$objects[[i]]
}

`[[<-.TypeEnvironment` = function(x, i, value) {
  x$objects[[i]] = value
  x
}

length.TypeEnvironment = function(x) {
  length(x$objects)
}

names.TypeEnvironment = function(x) {
  names(x$objects)
}
