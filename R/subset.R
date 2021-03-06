# General methods to take subsets.

#' @include terms.R
#' @include substitution.R
NULL


#' @export
setMethod("[[", signature("typesys::Substitution"),
  function(x, i, ...) {
    x@map[[i, ...]]
  })


#' @export
setMethod("[[", signature("typesys::Substitution", "typesys::Variable"),
  function(x, i, ...) {
    x@map[[i@name, ...]]
  })


#' @export
setMethod("[[<-", signature("typesys::Substitution"),
  function(x, i, ..., value) {
    x@map[[i, ...]] = value
    validObject(x)
    x
  })


#' @export
setMethod("[[<-", signature("typesys::Substitution", "typesys::Variable"),
  function(x, i, ..., value) {
    x@map[[i@name, ...]] = value
    validObject(x)
    x
  })


#' @export
setMethod("[", signature("typesys::Substitution"),
  function(x, i, ...) {
    x@map[i, ...]
  })


#' @export
setMethod("[<-", signature("typesys::Substitution"),
  function(x, i, ..., value) {
    x@map[i, ...] = value
    validObject(x)
    x
  })
