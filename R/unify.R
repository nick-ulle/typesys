#' Robinson's Unification Algorithm
#' 
#' Given two types, this method returns a substitution that can be applied to
#' both to make them equal. If no such substitution exists, the method throws
#' an error.
#'
#' @export
setGeneric("unify",
  function(x, y, sub = Substitution()) {
    x = applySubstitution(x, sub)
    y = applySubstitution(y, sub)

    standardGeneric("unify")
})


#' @export
setMethod("unify",
  signature(x = "typesys::AtomicType", y = "typesys::AtomicType"),
  function(x, y, sub) {
    if (!is(y, class(x)[[1]]))
      stop("Cannot unify.")
    
    list()
  }
)

#' @export
setMethod("unify",
  signature(x = "typesys::FunctionType", y = "typesys::FunctionType"),
  function(x, y, sub) {
    for (i in seq_along(x@args)) {
      sub = unify(x@args[[i]], y@args[[i]], sub)
    }
    
    unify(x@return_type, y@return_type, sub)
  }
)

#' @export
setMethod("unify",
  signature(x = "typesys::TypeVar", y = "ANY"),
  function(x, y, sub) unify(y, x, sub)
)

#' @export
setMethod("unify",
  signature(x = "typesys::TypeVar", y = "typesys::TypeVar"),
  function(x, y, sub) {
    if (x@name == y@name)
      sub
    else
      compose(sub, Substitution(structure(list(x), names = y@name)))
  }
)

#' @export
setMethod("unify",
  signature(x = "typesys::AtomicType", y = "typesys::TypeVar"),
  function(x, y, sub) {
    sub2 = Substitution(structure(list(x), names = y@name))
    compose(sub, sub2)
  }
)

#' @export
setMethod("unify",
  signature(x = "typesys::Join", y = "typesys::TypeVar"),
  function(x, y, sub) {
    x = compute(x)
    if (is(x, "typesys::Join"))
      callNextMethod()
    else
      unify(x, y, sub)
  }
)

#' @export
setMethod("unify",
  signature(x = "ANY", y = "typesys::TypeVar"),
  function(x, y, sub) {
    # Occurs check
    if (y %in% x)
      stop("Occurs check failed!")

    compose(sub, Substitution(y@name, x))
  }
)
