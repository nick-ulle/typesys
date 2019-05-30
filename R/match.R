#' @include terms.R
NULL

#' @export
setGeneric("%in%", valueClass = "logical")

#' @export
setMethod("%in%", signature("typesys::Variable", "ANY"),
  function(x, table) names(x) %in% table)

setMethod("%in%", signature("typesys::Variable", "typesys::Term"),
  function(x, table) names(x) %in% names(table))

## #' @export
## setMethod("%in%",
##   signature(x = "character", table = "TypeEnvironment"),
## function(x, table) {
##   # NOTE: This is not vectorized because for large type environments, breaking
##   # out of the inner loop early may be more efficient.
##   in_table = vapply(x, function(x_i) {
##     in_table = FALSE
##     for (type in table$objects) {
##       in_table = in_table | (x_i %in% type)
##       if (in_table) break
##     }
## 
##     in_table
##   }, NA, USE.NAMES = FALSE)
## 
##   in_table
## })
## 
## #' @export
## setMethod("%in%",
##   signature(x = "character", table = "typesys::AtomicType"),
## function(x, table) rep(FALSE, length(x))
## )
## 
## #' @export
## setMethod("%in%",
##   signature(x = "character", table = "typesys::TypeVariable"),
## function(x, table) x == table@name
## )
## 
## #' @export
## setMethod("%in%",
##   signature(x = "character", table = "typesys::FunctionType"),
## function(x, table) {
##   in_table = x %in% table@return_type
##   for (arg in table@args)
##     in_table = in_table | (x %in% arg)
## 
##   in_table
## })
## 
## #' @export
## setMethod("%in%",
##   signature(x = "typesys::TypeVariable", table = "typesys::Join"),
## function(x, table) {
##   in_table = rep(FALSE, length(x))
##   for (arg in table@args)
##     in_table = in_table | (x %in% arg)
## 
##   in_table
## })