# General format, print, and show methods.

.print = function(x, ...) cat(format(x, ...), "\n")

.show = function(object) cat(format(object, indent = 0), "\n")

format_class =
function(x, ..., strip_namespace = TRUE)
{
  str = class(x)[[1L]]

  if (strip_namespace) {
    # Split off "typesys::"
    str = strsplit(str, "::", fixed = TRUE)[[1L]]
    str = str[length(str)]
  }

  sprintf("%s", str)
}


#' Format an Object for Printing
#'
#' This function converts an object to a character string.
#'
#' @export
setGeneric("format")


# Substitutions ----------------------------------------
#' @export
print.Substitution = .print


#' @export
format.Substitution = function(x, ...) {
  if (length(x) == 0)
    return("Substitution (0 elements)\n")

  vals = vapply(x, format, "")
  sub = paste0(sprintf("%s ↦ %s", names(x), vals), collapse = "\n")

  sprintf("Substitution (%i elements)\n%s\n", length(x), sub)
}
