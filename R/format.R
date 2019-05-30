#' @include terms.R
#' @include type_environment.R
NULL

.print = function(x, ...) cat(format(x, ...), "\n")

.show = function(object) cat(format(object, indent = 0), "\n")

default_class_format = function(x, ...) {
  name = class(x)[[1L]]
  sprintf("<%s>", name)
}

#' @export
format.Substitution = function(x, ...) {
  if (length(x) == 0)
    return("Substitution (0 elements)\n")

  vals = vapply(x, format, "")
  sub = paste0(sprintf("%s ↦ %s", names(x), vals), collapse = "\n")

  sprintf("Substitution (%i elements)\n%s\n", length(x), sub)
}

#' @export
print.Substitution = .print

#' @export
format.TypeEnvironment = function(x, ...) {
  if (length(x) == 0)
    return("TypeEnvironment (0 elements)\n")

  vals = vapply(x$objects, format, NA_character_)
  a = paste0(sprintf("%s: %s", names(x), vals), collapse = "\n")

  sprintf("TypeEnvironment (%i elements)\n%s\n", length(x), a)
}

#' @export
print.TypeEnvironment = .print

setMethod("format", signature("typesys::Equality"),
  function(x, ...) {
    sprintf("%s ~= %s", format(x@t1), format(x@t2))
  })

setMethod("format", signature("typesys::ImplicitInstance"),
  function(x, ...) {
    sprintf("%s <= %s", format(x@t1), format(x@t2))
  })

setMethod("show", signature("typesys::Constraint"), .show)

setMethod("format", signature("typesys::Term"), default_class_format)

#' @export
setMethod("format", signature("typesys::Variable"),
  function(x, ...) {
    x@name
  })

#' @export
setMethod("format", signature("typesys::Composite"),
  function(x, ...) {
    # Split off "typesys::"
    name = class(x)[[1L]]
    name = substr(name, 10L, nchar(name))

    comp = vapply(x@components, format, NA_character_)
    sprintf("%s[%s]", comp, paste(args, collapse = ", "))
  })

setMethod("format", signature("typesys::Function"),
  function(x, ...) {
    comp = vapply(x@components, format, NA_character_)
    len = length(comp)

    sprintf("%s → %s", paste(comp[-len], collapse = ", "), comp[len])
  })

# ------------------------------------------------------------
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
## 
## #' @export
## setMethod("format", signature(x = "typesys::Type"),
##   function(x, indent = 0, ...) {
##     tag = class(x)[[1]]
##     substr(tag, 10, nchar(tag) - 4)
##   }
## )
##
## setMethod("show", signature(object = "typesys::Type"), .show)


#' @export
setMethod("show", signature(object = "typesys::Term"), .show)


format_quantified = function(quantified) {
  if (length(quantified) == 0)
    ""
  else
    sprintf("∀%s. ", paste(quantified, collapse = ", "))
}
