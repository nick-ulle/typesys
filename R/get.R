# Getters and setters for S4 classes.

#' @export
setGeneric("args")

#' @export
setMethod("args", signature("typesys::Function"),
  function(name) {
    len = length(name@components)
    name@components[-len]
  })


#' @export
setGeneric("return_type", function(fn) standardGeneric("return_type"),
  valueClass = "typesys::Term")

#' @export
setMethod("return_type", signature("typesys::Function"),
  function(fn) {
    len = length(fn@components)
    fn@components[[len]]
  })
