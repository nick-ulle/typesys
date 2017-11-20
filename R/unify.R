#' Robinson's Unification Algorithm
#' 
#' Given two types, this method returns a substitution that can be applied to
#' both to make them equal. If no such substitution exists, the method throws
#' an error.
#'
#' @export
setGeneric("unify",
  function(x, y, sub = Substitution()) {
    x = do_substitution(x, sub)
    y = do_substitution(y, sub)

    standardGeneric("unify")
})


# This function emits an error for incompatible types.
unification_error = function(x, y, sub) {
  msg = sprintf("Cannot unify types '%s' and '%s'.", format(x), format(y))
  stop(msg, call. = FALSE)
}


# Atomic Types ----------------------------------------

#' @export
setMethod("unify",
  signature(x = "typesys::AtomicType", y = "typesys::AtomicType"),
  function(x, y, sub) {
    if (class(x)[1] == class(y)[1])
      sub
    else
      unification_error(x, y)
  }
)

#' @export
setMethod("unify", c(x = "typesys::AtomicType", y = "ANY"), unification_error)

#' @export
setMethod("unify", c(x = "ANY", y = "typesys::AtomicType"), unification_error)


# Type Variables ----------------------------------------

#' @export
setMethod("unify",
  signature(x = "typesys::TypeVariable", y = "typesys::TypeVariable"),
  function(x, y, sub) {
    if (x@name == y@name)
      sub
    else
      compose(sub, Substitution(setNames(list(x), y@name)))
  }
)

#' @export
setMethod("unify",
  signature(x = "typesys::TypeVariable", y = "ANY"),
  # Swap order if type variable is on the left-hand side only.
  function(x, y, sub) unify(y, x, sub)
)

#' @export
setMethod("unify",
  signature(x = "typesys::Join", y = "typesys::TypeVariable"),
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
  signature(x = "ANY", y = "typesys::TypeVariable"),
  function(x, y, sub) {
    if (y %in% x) {
      unification_error(x, y)
    }

    compose(sub, Substitution(setNames(list(x), y@name)))
  }
)


# Composite Types ----------------------------------------

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
