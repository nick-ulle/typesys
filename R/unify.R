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
      compose(sub, Substitution(y@name, x))
  }
)

#' @export
setMethod("unify",
  signature(x = "typesys::AtomicType", y = "typesys::TypeVar"),
  function(x, y, sub) compose(sub, Substitution(y@name, x))
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



#' @export
setGeneric("%in%", valueClass = "logical")

#' @export
setMethod("%in%",
  signature(x = "typesys::TypeVar", table = "typesys::AtomicType"),
  function(x, table) FALSE
)

#' @export
setMethod("%in%",
  signature(x = "typesys::TypeVar", table = "typesys::TypeVar"),
  function(x, table) x@name == table@name
)

#' @export
setMethod("%in%",
  signature(x = "typesys::TypeVar", table = "typesys::FunctionType"),
  function(x, table) {
    args = vapply(table@args, `%in%`, NA, x)

    any(args) || (x %in% table@return_type)
  }
)