#' @include types.R
#' @include type_environment.R
NULL

.print = function(x, ...) cat(format(x, ...), "\n")

.show = function(object) cat(format(object, indent = 0), "\n")

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


#' @export
setMethod("format", signature(x = "typesys::TypeVariable"),
  function(x, indent = 0, top = TRUE, ...) {
    if (top)
      paste0(format_quantified(x@quantified), x@name)
    else
      x@name
  }
)

#' @export
setMethod("format", signature(x = "typesys::FunctionType"),
  function(x, indent = 0, top = TRUE, ...) {
    args = vapply(x@args, format, "", top = FALSE)
    args = paste(args, collapse = ", ")

    template =
      if (length(x@args) == 0) "(%s) → %s"
      else "%s → %s"

    str = sprintf(template, args, format(x@return_type, top = FALSE))

    if (top)
      paste0(format_quantified(x@quantified), str)
    else
      str
  }
)

#' @export
setMethod("format", signature(x = "typesys::Join"),
  function(x, indent = 0, ...) {
    args = vapply(x@args, format, NA_character_)
    args = paste0(args, collapse = ", ")
    sprintf("Join(%s)", args)
  }
)

#' @export
setMethod("format", signature(x = "typesys::CompositeType"),
  function(x, indent = 0, ...) {
    types_msg = vapply(x@types, format, NA_character_, indent = indent + 2)
    types_msg = paste0(types_msg, collapse = "\n")

    sprintf("%s\n%s", callNextMethod(), types_msg)
  }
)

#' @export
setMethod("format", signature(x = "typesys::Type"),
  function(x, indent = 0, ...) {
    tag = class(x)[[1]]
    substr(tag, 10, nchar(tag) - 4)
  }
)


#' @export
setMethod("show", signature(object = "typesys::Type"), .show)


format_quantified = function(quantified) {
  if (length(quantified) == 0)
    ""
  else
    sprintf("∀%s. ", paste(quantified, collapse = ", "))
}
