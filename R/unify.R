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
    # FIXME: Check that both have the same arity.
    for (i in seq_along(x@args)) {
      # NOTE: Here is where unification run-time is exponential. If pairs of
      # terms are repeated, we repeat the unification.
      sub = unify(x@args[[i]], y@args[[i]], sub)
    }
    
    unify(x@return_type, y@return_type, sub)
  }
)

#' @export
setMethod("unify", c(x = "typesys::RecordType", y = "typesys::RecordType"),
  function(x, y, sub) {
    # Require them to have the same fields, but not necessarily the same order.
    names_x = names(x@fields)
    names_y = names(y@fields)
    if (length(names_x) != length(names_y) || sort(names_x) != sort(names_y))
      unification_error(x, y)

    for (name in names_x)
      sub = unify(x@fields[[name]], y@fields[[name]], sub)

    sub
  }
)

#' @export
setMethod("unify",
  c(x = "typesys::ParameterType", y = "typesys::ParameterType"),
function(x, y, sub) {
  fields_x = x@fields
  fields_y = y@fields

  # Match parameters by name.
  names_xy = intersect(names(fields_x), names(fields_y))
  names_xy = names_xy[names_xy != ""]
  if (length(names_xy) > 0) {
    idx_x = match(names_xy, names(fields_x))
    idx_y = match(names_xy, names(fields_y))

    for (i in seq_along(names_xy)) {
      i_x = idx_x[i]
      i_y = idx_y[i]
      sub = unify(fields_x[[i_x]], fields_y[[i_y]], sub)
    }

    fields_x = fields_x[-idx_x]
    fields_y = fields_y[-idx_y]
  }

  # Check extra parameters are optional.
  if (length(fields_x) > length(fields_y)) {
    len = length(fields_y)
    fields_x = head(fields_x, len)
    extra = tail(fields_x, -len)
  } else {
    len = length(fields_x)
    fields_y = head(fields_y, len)
    extra = tail(fields_y, -len)
  }

  # TODO:

  # Match remaining parameters by position.
  if (!all(names(fields_x) == "" | names(fields_y) == ""))
    unification_error(x, y)

  for (i in seq_along(fields_x))
    sub = unify(fields_x[[i]], fields_y[[i]], sub)

  sub
})
