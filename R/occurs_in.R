#' @include terms.R


#' @export
setGeneric("%occurs_in%", function(x, y) standardGeneric("%occurs_in%"))


#' @export
setMethod("%occurs_in%", signature("typesys::Variable", "typesys::Term"),
function(x, y)
{
  names(x) %in% names(y)
})
