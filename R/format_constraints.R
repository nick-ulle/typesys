# Format methods for Constraint objects.

#' @include format.R
#' @include constraints.R
NULL


#' @export
setMethod("show", signature("typesys::Constraint"), .show)


#' @export
setMethod("format", signature("typesys::Equivalence"),
  function(x, ...) {
    sprintf("%s == %s", format(x@t1), format(x@t2))
  })


#' @export
setMethod("format", signature("typesys::ImplicitInstance"),
  function(x, ...) {
    t1 = format(x@t1)
    t2 = format(x@t2)

    if (length(x@monomorphic) == 0L)
      return (sprintf("%s instance of %s", t1, t2))

    m = vapply(x@monomorphic, format, NA_character_)
    m = paste0(m, collapse = ", ")
    sprintf("%s instance of %s; monomorphic %s", t1, t2, m)
  })
