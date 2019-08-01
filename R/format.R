# General format, print, and show methods.

.print = function(x, ...) cat(format(x, ...), "\n")

.show = function(object) cat(format(object, indent = 0), "\n")

default_class_format = function(x, ...) {
  name = class(x)[[1L]]
  sprintf("<%s>", name)
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
