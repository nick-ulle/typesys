#' @include types.R
NULL

.print = function(x, ...) cat(format(x, ...), "\n")


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
setMethod("show", signature(object = "typesys::Type"),
  function(object) cat(format(object, indent = 0), "\n")
)

#' @export
setMethod("format", signature(x = "typesys::TypeVar"),
  function(x, indent = 0, ...) {
    if (x@quantified)
      sprintf("∀%s", x@name)
    else
      x@name
  }
)

# Print out 
#   RecordType ()
#     IntegerType (index)

#' @export
setMethod("format", signature(x = "typesys::Type"),
  function(x, indent = 0, ...) {
    tag = class(x)[[1]]
    substr(tag, 10, nchar(tag) - 4)
  }
)


#' @export
setMethod("format", signature(x = "typesys::CompositeType"),
  function(x, indent = 0, ...) {
    types_msg = vapply(x@types, format, character(1), indent = indent + 2)
    types_msg = paste0(types_msg, collapse = "\n")

    sprintf("%s\n%s", callNextMethod(), types_msg)
  }
)

#' @export
setMethod("format", signature(x = "typesys::FunctionType"),
  function(x, indent = 0, ...) {
    args = vapply(x@args, format, "")
    args = paste(args, collapse = ", ")

    template =
      if (length(x@args) == 0) "(%s) → %s"
      else "%s → %s"
    sprintf(template, args, format(x@returnVal))
  }
)


