#' Robinson's Unification Algorithm
#' 
#' Given two types, this method returns a substitution that can be applied to
#' both to make them equal. If no such substitution exists, the method throws
#' an error.
#'
#' @export
setGeneric("unify",
  function(e1, e2, sub = Substitution(), ...) {
    e1 = sub(e1)
    if (!missing(e2))
      e2 = sub(e2)

    standardGeneric("unify")
  })


# This function emits an error for incompatible types.
unification_error = function(e1, e2) {
  msg = sprintf("Cannot unify types '%s' and '%s'.", format(e1), format(e2))
  stop(msg, call. = FALSE)
}

# Constraints ----------------------------------------

#' @export
setMethod("unify", signature("typesys::Constraint"),
  function(e1, e2, sub, ...) {
    unify(e1@t1, e1@t2, sub, ...)
  }
)

#' @export
setMethod("unify", signature("typesys::ImplicitInstance"),
  function(e1, e2, sub, ..., counter = NULL) {
    # Generalize and instantiate t2.
    e1 = instantiate(e1, counter = counter)
    unify(e1@t1, e1@t2, sub, ...)
  }
)


# Atomic Types ----------------------------------------

#' @export
setMethod("unify", signature("typesys::Constant", "typesys::Constant"),
  function(e1, e2, sub, ...) {
    if (e1 == e2)
      sub
    else
      unification_error(e1, e2)
  }
)


# Type Variables ----------------------------------------

#' @export
setMethod("unify", signature("typesys::Variable", "typesys::Variable"),
  function(e1, e2, sub, ...) {
    if (e1 == e2) # same variable
      sub
    else
      make_substitution(e1, e2)(sub)
  }
)

#' @export
setMethod("unify", signature("typesys::Term", "typesys::Variable"),
  # Swap order if variable is on the right-hand side only.
  function(e1, e2, sub, ...) unify(e2, e1, sub, ...)
)

#' @export
setMethod("unify", signature("typesys::Variable", "typesys::Term"),
  function(e1, e2, sub, ...) {
    # OCCURS CHECK
    #
    # Check whether e1 occurs in e2; if it does, the substitution {e1 -> e2} is
    # circular, so unification fails.
    if (e1 %in% e2)
      unification_error(e1, e2)
    else
      make_substitution(e1, e2)(sub)
  }
)

## #' @export
## setMethod("unify",
##   signature(x = "typesys::Join", y = "typesys::TypeVariable"),
##   function(x, y, sub) {
##     x = compute(x)
##     if (is(x, "typesys::Join"))
##       callNextMethod()
##     else
##       unify(x, y, sub)
##   }
## )


# Composite Types ----------------------------------------

#' @export
setMethod("unify", signature("typesys::Composite", "typesys::Composite"),
  function(e1, e2, sub, ...) {
    len = length(e1@components)
    if (len != length(e2@components))
      unification_error(e1, e2)

    # NOTE: This loop makes unification exponential in the number of terms. We
    # unify for every component, even if some components are identical.
    for (i in seq_len(len)) {
      new_sub = unify(e1@components[[i]], e2@components[[i]], sub)
      sub = new_sub(sub)
    }

    sub
  })

## #' @export
## setMethod("unify",
##   signature(x = "typesys::FunctionType", y = "typesys::FunctionType"),
##   function(x, y, sub) {
##     # FIXME: Check that both have the same arity.
##     for (i in seq_along(x@args)) {
##       # NOTE: Here is where unification run-time is exponential. If pairs of
##       # terms are repeated, we repeat the unification.
##       sub = unify(x@args[[i]], y@args[[i]], sub)
##     }
##     
##     unify(x@return_type, y@return_type, sub)
##   }
## )
## 
## #' @export
## setMethod("unify", c(x = "typesys::RecordType", y = "typesys::RecordType"),
##   function(x, y, sub) {
##     # Require them to have the same fields, but not necessarily the same order.
##     names_x = names(x@fields)
##     names_y = names(y@fields)
##     if (length(names_x) != length(names_y) || sort(names_x) != sort(names_y))
##       unification_error(x, y)
## 
##     for (name in names_x)
##       sub = unify(x@fields[[name]], y@fields[[name]], sub)
## 
##     sub
##   }
## )
## 
## #' @export
## setMethod("unify",
##   c(x = "typesys::ParameterType", y = "typesys::ParameterType"),
## function(x, y, sub) {
##   fields_x = x@fields
##   fields_y = y@fields
## 
##   # Match parameters by name.
##   names_xy = intersect(names(fields_x), names(fields_y))
##   names_xy = names_xy[names_xy != ""]
##   if (length(names_xy) > 0) {
##     idx_x = match(names_xy, names(fields_x))
##     idx_y = match(names_xy, names(fields_y))
## 
##     for (i in seq_along(names_xy)) {
##       i_x = idx_x[i]
##       i_y = idx_y[i]
##       sub = unify(fields_x[[i_x]], fields_y[[i_y]], sub)
##     }
## 
##     fields_x = fields_x[-idx_x]
##     fields_y = fields_y[-idx_y]
##   }
## 
##   # Check extra parameters are optional.
##   if (length(fields_x) > length(fields_y)) {
##     len = length(fields_y)
##     fields_x = head(fields_x, len)
##     extra = tail(fields_x, -len)
##   } else {
##     len = length(fields_x)
##     fields_y = head(fields_y, len)
##     extra = tail(fields_y, -len)
##   }
## 
##   # TODO:
## 
##   # Match remaining parameters by position.
##   if (!all(names(fields_x) == "" | names(fields_y) == ""))
##     unification_error(x, y)
## 
##   for (i in seq_along(fields_x))
##     sub = unify(fields_x[[i]], fields_y[[i]], sub)
## 
##   sub
## })
