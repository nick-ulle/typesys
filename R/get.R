# Getters and setters for S4 classes.

#' @export
setGeneric("args")

#' @export
setMethod("args", signature("typesys::Function"),
  function(name) {
    len = length(name@components)
    name@components[-len]
  })


# ----------------------------------------
#' @export
setGeneric("return_type", function(fn) standardGeneric("return_type"),
  valueClass = "typesys::Term")

#' @export
setMethod("return_type", signature("typesys::Function"),
  function(fn) {
    len = length(fn@components)
    fn@components[[len]]
  })


# ----------------------------------------
#' @export
setGeneric("vars", function(term) standardGeneric("vars"))
# TODO: Add tests for this generic.

#' @export
setMethod("vars", signature("typesys::Variable"),
  function(term) {
    term
  })

#' @export
setMethod("vars", signature("typesys::Constant"),
  function(term) {
    list()
  })

#' @export
setMethod("vars", signature("typesys::Composite"),
  function(term) {
    result = lapply(term@components, vars)
    result = unlist(result, recursive = FALSE, use.names = FALSE)
    # Make sure an empty list is returned instead of NULL.
    as.list(result)
  })
