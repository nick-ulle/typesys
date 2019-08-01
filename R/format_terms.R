# Format methods for Term objects.

#' @include format.R
#' @include terms.R
NULL


#' @export
setMethod("show", signature("typesys::Term"), .show)


#' @export
setMethod("format", signature("typesys::Term"), format_class)


#' @export
setMethod("format", signature("typesys::Variable"),
function(x, ...)
{
  x@name
})


#' @export
setMethod("format", signature("typesys::Composite"),
  function(x, ...) {
    name = format_class(x, strip_namespace = TRUE)

    comp = vapply(x@components, format, NA_character_)
    sprintf("%s[%s]", name, paste(comp, collapse = ", "))
  })


#' @export
setMethod("format", signature("typesys::Function"),
  function(x, ...) {
    comp = vapply(x@components, format, NA_character_)
    len = length(comp)

    sprintf("(%s) → %s", paste(comp[-len], collapse = ", "), comp[len])
  })


## #' @export
## setMethod("format", "typesys::RecordType",
## function(x, indent = 0, top = TRUE, ...) {
##   fields = vapply(x@fields, format, NA_character_, top = FALSE)
##   fields = sprintf("%s: %s", names(x@fields), fields)
##   fields = paste(fields, collapse = ", ")
## 
##   str = sprintf("(%s)", fields)
## 
##   if (top)
##     paste0(format_quantified(x@quantified), str)
##   else
##     str
## })
## 
## #' @export
## setMethod("format", "typesys::Join",
## function(x, indent = 0, ...) {
##   args = vapply(x@args, format, NA_character_)
##   args = paste0(args, collapse = ", ")
##   sprintf("Join(%s)", args)
## })
## 
## #' @export
## setMethod("format", signature(x = "typesys::ArrayType"),
##   function(x, indent = 0, ...) {
##     sprintf("%s(%s)", callNextMethod(), format(x@type))
##   }
## )
