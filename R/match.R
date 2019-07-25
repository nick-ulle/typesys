#' @include terms.R
NULL


#' @export
setGeneric("match")


#' @export
setMethod("match", signature("typesys::Variable", "ANY"),
function(x, table, nomatch = 0L)
{
  # TODO: It's unclear where this method is actually used, so mark as
  # deprecated and remove or migrate eventually.
  .Deprecated()

  base::match(names(x), table)
})


#' @export
setMethod("match", signature("typesys::Term", "list"),
function(x, table, nomatch = 0L)
{
  for (t in table)
    if (x == t)
      return (TRUE)

  FALSE
})


# NOTE: This makes `%in%` call the `match` generic instead of `base::match`.
#' @export
`%in%` =
function(x, table)
{
  match(x, table, nomatch = 0L) > 0L
}
