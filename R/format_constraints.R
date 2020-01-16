# Format methods for Constraint objects.

#' @include format.R
#' @include constraints.R
NULL


#' @export
setMethod("show", signature("typesys::Constraint"), .show)


#' @export
setMethod("format", signature("typesys::Equivalence"),
  function(x, ...) {
    t1 = format(x@t1)
    t2 = format(x@t2)
    src = toString(x@src)
    sprintf("%s == %s\n  from %s", t1, t2, src)
  })


#' @export
setMethod("format", signature("typesys::ImplicitInstance"),
  function(x, ...) {
    t1 = format(x@t1)
    t2 = format(x@t2)
    src = toString(x@src)

    if (length(x@monomorphic) == 0L)
      return (sprintf("%s instance of %s\n  from %s", t1, t2, src))

    m = vapply(x@monomorphic, format, NA_character_)
    m = paste0(m, collapse = ", ")
    sprintf("%s instance of %s; monomorphic %s\n  from %s", t1, t2, m, src)
  })
